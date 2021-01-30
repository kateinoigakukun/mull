#include "mull/Filters/GitDiffFilter.h"

#include "mull/Diagnostics/Diagnostics.h"
#include "mull/Toolchain/Runner.h"

#include "llvm/IR/InstIterator.h"
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/DebugLoc.h>
#include <llvm/IR/Function.h>
#include <llvm/Support/FileSystem.h>

#include <iostream>
#include <regex>
#include <sstream>
#include <unistd.h>

using namespace mull;

static std::string getCwd() {
  char cwd_buffer[8096];
  if (getcwd(cwd_buffer, sizeof(cwd_buffer)) == NULL) {
    return "";
  }
  return std::string(cwd_buffer);
}

GitDiffFilter::GitDiffFilter(Diagnostics &diagnostics)
    : diagnostics(diagnostics), gitDiffInfoStorage() {}

std::string GitDiffFilter::name() {
  return "Git Diff";
}

void GitDiffFilter::activateAgainstGitBranch(const std::string &gitDiffBranch) {
  /// Assuming that Mull is run from the project's directory.
  const std::string gitRootPath = getCwd();
  mull::GitDiffReader gitDiffReader(diagnostics, gitRootPath);

  mull::GitDiffInfo gitDiffInfo = gitDiffReader.readGitDiff(gitDiffBranch);

  gitDiffInfoStorage = gitDiffInfo;
}

bool GitDiffFilter::shouldSkip(llvm::Function *function) {
  auto debugInfo = llvm::dyn_cast<llvm::DISubprogram>(function->getMetadata(0));

  std::string unitDirectory;
  std::string unitFilePath;
  if (llvm::DICompileUnit *unit = debugInfo->getUnit()) {
    unitDirectory = unit->getDirectory();
    unitFilePath = unit->getFilename();
  }
  assert(unitFilePath[0] == '/');
  assert(llvm::sys::fs::exists(unitFilePath));

  std::string directory = debugInfo->getDirectory().str();
  std::string filePath = debugInfo->getFilename().str();
  int beginLine = debugInfo->getLine();
  int endLine = beginLine;

  /// LLVM IR does not contain end line information for Functions.
  /// Iterate over the instructions manually and find the maximum end line.
  for (llvm::inst_iterator instIterator = inst_begin(function), functionEnd = inst_end(function);
       instIterator != functionEnd;
       ++instIterator) {
    auto &instruction = *instIterator;
    if (instruction.getMetadata(0) == nullptr) {
      continue;
    }
    int line = instruction.getDebugLoc()->getLine();
    if (line > endLine) {
      endLine = line;
    }
  }
  return (shouldSkipSourceFunction(unitFilePath, beginLine, endLine));
}

bool GitDiffFilter::shouldSkipSourceFunction(const std::string &fullPath, const int beginLine,
                                             const int endLine) {
  /// No diff, not filtering.
  if (gitDiffInfoStorage.size() == 0) {
    return false;
  }

  /// If there is an intersection between the function's [begin, end] range and
  /// any of the diff's ranges, this function is NOT skipped.
  GitDiffSourceFileRanges ranges = gitDiffInfoStorage[fullPath];

  for (auto &range : ranges) {
    int rangeEnd = range.first + range.second - 1;
    if (range.first <= endLine && beginLine <= rangeEnd) {
      return false;
    }
  }

  return true;
}
