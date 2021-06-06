#pragma once

#include "CXXASTStorage.h"
#include "mull/JunkDetection/JunkDetector.h"

namespace mull {

class MutationPoint;

class CXXJunkDetector : public JunkDetector {
public:
  explicit CXXJunkDetector(CXXASTStorage &storage);
  ~CXXJunkDetector() override = default;

  bool isJunk(MutationPoint *point) override;

private:
  CXXASTStorage &astStorage;
};

} // namespace mull
