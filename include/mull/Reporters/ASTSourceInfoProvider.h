#pragma once

// TODO: This file will be removed by https://github.com/mull-project/mull/pull/873

namespace mull {

struct MutationPointSourceInfo {
  int beginLine = -1;
  int beginColumn = -1;
  int endLine = -1;
  int endColumn = -1;
};

class CXXASTStorage;
class Mutant;
class Diagnostics;

class SourceInfoProvider {
public:
  virtual MutationPointSourceInfo getSourceInfo(Mutant *mutant) = 0;
};

class NullSourceInfoProvider : public SourceInfoProvider {
public:
  MutationPointSourceInfo getSourceInfo(Mutant *mutant) override {
    return {};
  }
};

class ASTSourceInfoProvider : public SourceInfoProvider {
public:
  ASTSourceInfoProvider(Diagnostics &diagnostics, CXXASTStorage &astStorage);
  ~ASTSourceInfoProvider() = default;
  MutationPointSourceInfo getSourceInfo(Mutant *mutant) override;

//private:
  Diagnostics &diagnostics;
  CXXASTStorage &astStorage;
};

} // namespace mull
