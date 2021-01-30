#pragma once

#include "mull/Diagnostics/Diagnostics.h"
#include "mull/Filters/Filter.h"
#include "mull/Filters/FunctionFilter.h"
#include "mull/Filters/GitDiffReader.h"

namespace mull {
struct SourceLocation;
class GitDiffFilter : public FunctionFilter {
public:
  GitDiffFilter(Diagnostics &diagnostics);

  std::string name() override;

  void activateAgainstGitBranch(const std::string &gitDiffBranch);

  bool shouldSkip(llvm::Function *function) override;

  bool shouldSkipSourceFunction(const std::string &fullPath, const int beginLine,
                                const int endLine);

private:
  Diagnostics &diagnostics;
  GitDiffInfo gitDiffInfoStorage;
};
} // namespace mull
