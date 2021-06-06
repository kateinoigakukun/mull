#pragma once

#include "mull/Diagnostics/Diagnostics.h"
#include "mull/Filters/MutationFilter.h"
#include "mull/JunkDetection/CXX/CXXASTStorage.h"

namespace mull {

class ASTMutationFilter: public MutationFilter {
public:
  ASTMutationFilter(Diagnostics &diagnostics, ASTMutationStorage &astStorage);
  bool shouldSkip(MutationPoint *point) override ;
  std::string name() override;

private:
  Diagnostics &diagnostics;
  ASTMutationStorage &astStorage;
};
}
