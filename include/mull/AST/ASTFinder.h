#pragma once

#include "mull/Mutators/MutatorKind.h"

namespace mull {

struct Configuration;
class Diagnostics;
class Program;
class CXXASTStorage;
class FilePathFilter;

class ASTFinder {
public:
  ASTFinder(const MutatorKindSet &mutatorKindSet);

  void findMutations(Diagnostics &diagnostics, const Configuration &config, Program &program,
                     FilePathFilter &pathFilter, CXXASTStorage &storage);

private:
  const mull::MutatorKindSet &mutatorKindSet;
};
} // namespace mull
