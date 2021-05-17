#pragma once

#include <mull/Mutators/Mutator.h>

namespace mull {
namespace swift {

class SwiftLogicalAndToOr : public mull::Mutator {

public:
  static std::string ID();
  static std::string description();

  std::string getUniqueIdentifier() override {
    return ID();
  }
  std::string getUniqueIdentifier() const override {
    return ID();
  }
  std::string getDescription() const override {
    return description();
  }
  mull::MutatorKind mutatorKind() override {
    return mull::MutatorKind::Swift_Logical_AndToOr;
  }
  std::string getDiagnostics() const override {
    return "Replaced && with ||";
  }
  std::string getReplacement() const override {
    return "||";
  }

  void applyMutation(llvm::Function *function, const mull::MutationPointAddress &address,
                     irm::IRMutation *lowLevelMutation) override;

  std::vector<mull::MutationPoint *> getMutations(mull::Bitcode *bitcode,
                                                  const mull::FunctionUnderTest &function) override;
};

class SwiftEqualToNotEqual : public mull::Mutator {

public:
  static std::string ID();
  static std::string description();

  std::string getUniqueIdentifier() override {
    return ID();
  }
  std::string getUniqueIdentifier() const override {
    return ID();
  }
  std::string getDescription() const override {
    return description();
  }
  mull::MutatorKind mutatorKind() override {
    return mull::MutatorKind::Swift_EqualToNotEqual;
  }
  std::string getDiagnostics() const override {
    return "Replaced == with !=";
  }
  std::string getReplacement() const override {
    return "==";
  }

  void applyMutation(llvm::Function *function, const mull::MutationPointAddress &address,
                     irm::IRMutation *lowLevelMutation) override;

  std::vector<mull::MutationPoint *> getMutations(mull::Bitcode *bitcode,
                                                  const mull::FunctionUnderTest &function) override;
};


class SwiftNotEqualToEqual : public mull::Mutator {

public:
  static std::string ID();
  static std::string description();

  std::string getUniqueIdentifier() override {
    return ID();
  }
  std::string getUniqueIdentifier() const override {
    return ID();
  }
  std::string getDescription() const override {
    return description();
  }
  mull::MutatorKind mutatorKind() override {
    return mull::MutatorKind::Swift_NotEqualToEqual;
  }
  std::string getDiagnostics() const override {
    return "Replaced != with ==";
  }
  std::string getReplacement() const override {
    return "!=";
  }

  void applyMutation(llvm::Function *function, const mull::MutationPointAddress &address,
                     irm::IRMutation *lowLevelMutation) override;

  std::vector<mull::MutationPoint *> getMutations(mull::Bitcode *bitcode,
                                                  const mull::FunctionUnderTest &function) override;
};

} // namespace swift
} // namespace mull
