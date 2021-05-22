#include "mull/FunctionUnderTest.h"
#include "mull/MutationPoint.h"
#include "mull/Mutators/Swift/SwiftMutations.h"
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>

using namespace mull;
using namespace mull::swift;

namespace mull::swift {
class EquatableOperationFinder {
public:
  enum Operator { Equal, NotEqual };
  Operator op;

  mull::Bitcode *bitcode;
  mull::Mutator *mutator;

  enum SimpleFinderState {
    LOOKING_ICMP_EQ,
    FOUND_ICMP_EQ,
    FOUND_XOR,
  };
  SimpleFinderState simpleState = LOOKING_ICMP_EQ;
  llvm::Instruction *foundICMP = nullptr;
  llvm::Instruction *foundXor = nullptr;
  llvm::Instruction *prevInst = nullptr;

  EquatableOperationFinder(Operator op, mull::Bitcode *bitcode, mull::Mutator *mutator)
      : op(op), bitcode(bitcode), mutator(mutator) {}

  std::vector<MutationPoint *> getMutations(const FunctionUnderTest &function);

private:
  void nextSimpleState(const llvm::Instruction &instruction,
                       std::vector<MutationPoint *> &mutations);
  void nextInlinedState(const llvm::Instruction &instruction,
                        std::vector<MutationPoint *> &mutations);
};
} // namespace mull::swift

void EquatableOperationFinder::nextSimpleState(const llvm::Instruction &instruction,
                                               std::vector<MutationPoint *> &mutations) {
  //     For (op == Equal)
  //
  //     /---> LOOKING_ICMP_EQ ------\
  //     |                           | (icmp eq)
  //     |                           v
  // FOUND_EQ <----------------- FOUND_ICMP_EQ
  //          (different dbg node)
  //
  //
  //     For (op == NotEqual)
  //
  //                (icmp eq)
  // LOOKING_ICMP_EQ ------> FOUND_ICMP_EQ
  //     ^                           |
  //     |                           |
  //     |                           v
  // FOUND_NEQ <----------------- FOUND_XOR
  //          (different dbg node)
  //

  switch (simpleState) {
  case LOOKING_ICMP_EQ: {
    if (auto icmp = llvm::dyn_cast<llvm::ICmpInst>(&instruction)) {
      if (icmp->getPredicate() == llvm::CmpInst::ICMP_EQ) {
        simpleState = FOUND_ICMP_EQ;
      }
    }
    break;
  }
  case FOUND_ICMP_EQ: {
    assert(prevInst);
    foundICMP = prevInst;
    switch (op) {
    case Equal: {
      if (instruction.getDebugLoc() != prevInst->getDebugLoc()) {
        mutations.push_back(new mull::MutationPoint(mutator, nullptr, foundICMP, bitcode));
        simpleState = LOOKING_ICMP_EQ;
      } else {
        simpleState = LOOKING_ICMP_EQ;
      }
      break;
    }
    case NotEqual: {
      if (auto binOp = llvm::dyn_cast<llvm::BinaryOperator>(&instruction)) {
        if (binOp->getOpcode() == llvm::Instruction::Xor &&
            instruction.getDebugLoc() == prevInst->getDebugLoc()) {
          simpleState = FOUND_XOR;
        } else {
          simpleState = LOOKING_ICMP_EQ;
        }
      } else {
        simpleState = LOOKING_ICMP_EQ;
      }
    }
    default:
      break;
    }
    break;
  }
  case FOUND_XOR: {
    assert(prevInst);
    foundXor = prevInst;
    if (instruction.getDebugLoc() != prevInst->getDebugLoc()) {
      mutations.push_back(new mull::MutationPoint(mutator, nullptr, foundICMP, bitcode));
      simpleState = LOOKING_ICMP_EQ;
    } else {
      simpleState = LOOKING_ICMP_EQ;
    }
  }
  default:
    llvm_unreachable("invalid state");
  }
}

std::vector<MutationPoint *>
EquatableOperationFinder::getMutations(const FunctionUnderTest &function) {
  assert(bitcode);
  std::vector<MutationPoint *> mutations;

  auto instructions = llvm::instructions(function.getFunction());

  for (auto it = instructions.begin(); it != instructions.end(); it++) {
    auto &instruction = *it;
    nextSimpleState(instruction, mutations);

    prevInst = &instruction;
  }
  return mutations;
}

std::string SwiftEqualToNotEqual::ID() {
  return "swift_eq_to_ne";
}

std::string SwiftEqualToNotEqual::description() {
  return "Replaces == with !=";
}

std::vector<MutationPoint *> SwiftEqualToNotEqual::getMutations(Bitcode *bitcode,
                                                                const FunctionUnderTest &function) {
  swift::EquatableOperationFinder finder(EquatableOperationFinder::Equal, bitcode, this);
  return finder.getMutations(function);
}

void SwiftEqualToNotEqual::applyMutation(llvm::Function *function,
                                         const mull::MutationPointAddress &address,
                                         irm::IRMutation *lowLevelMutation) {
  llvm::Instruction &inst = address.findInstruction(function);
  llvm::ICmpInst *icmp = llvm::dyn_cast<llvm::ICmpInst>(&inst);
  assert(icmp);
  icmp->setPredicate(llvm::CmpInst::ICMP_NE);
}

std::string SwiftNotEqualToEqual::ID() {
  return "swift_ne_to_eq";
}

std::string SwiftNotEqualToEqual::description() {
  return "Replaces != with ==";
}

std::vector<MutationPoint *> SwiftNotEqualToEqual::getMutations(Bitcode *bitcode,
                                                                const FunctionUnderTest &function) {
  swift::EquatableOperationFinder finder(EquatableOperationFinder::NotEqual, bitcode, this);
  return finder.getMutations(function);
}

void SwiftNotEqualToEqual::applyMutation(llvm::Function *function,
                                         const mull::MutationPointAddress &address,
                                         irm::IRMutation *lowLevelMutation) {
  llvm::Instruction &inst = address.findInstruction(function);
  llvm::ICmpInst *icmp = llvm::dyn_cast<llvm::ICmpInst>(&inst);
  assert(icmp);
  icmp->setPredicate(llvm::CmpInst::ICMP_NE);
}
