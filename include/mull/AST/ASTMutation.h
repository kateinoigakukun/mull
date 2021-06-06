#pragma once

#include <mull/Mutators/Mutator.h>

namespace clang {
  class Stmt;
}

namespace mull {

struct ASTMutation {
  mull::MutatorKind mutatorKind;
  int line;
  int column;
  ASTMutation(MutatorKind mutatorKind, int line, int column)
      : mutatorKind(mutatorKind), line(line), column(column) {}
};

} // namespace mull
