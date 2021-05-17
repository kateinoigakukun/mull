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
  enum Operator {
    Equal, NotEqual
  };

  Operator op;

  EquatableOperationFinder(Operator op) : op(op) {}
  std::vector<MutationPoint *> getMutations(Bitcode *bitcode,
                                            const FunctionUnderTest &function,
                                            mull::Mutator *mutator);
};
}

std::vector<MutationPoint *> EquatableOperationFinder::getMutations(Bitcode *bitcode,
                                                                    const FunctionUnderTest &function,
                                                                    mull::Mutator *mutator) {
  assert(bitcode);
  std::vector<MutationPoint *> mutations;

  // LOOKING_ICMP_EQ (icmp)-> FOUND_ICMP_EQ (different dbg node)-> FOUND_EQ -> LOOKING_ICMP_EQ
  //                                        (xor               )-> FOUND_XOR (different dbg node)-> FOUND_NEQ -> LOOKING_ICMP_EQ
  enum {
    LOOKING_ICMP_EQ,
    FOUND_ICMP_EQ,
    FOUND_XOR,
  } state = LOOKING_ICMP_EQ;
  llvm::Instruction *foundICMP = nullptr;
  llvm::Instruction *foundXor = nullptr;
  llvm::Instruction *prevInst = nullptr;
  auto instructions = llvm::instructions(function.getFunction());

  for (auto it = instructions.begin(); it != instructions.end(); it++) {
    auto &instruction = *it;
    switch (state) {
      case LOOKING_ICMP_EQ: {
        if (auto icmp = llvm::dyn_cast<llvm::ICmpInst>(&instruction)) {
          if (icmp->getPredicate() == llvm::CmpInst::ICMP_EQ) {
            state = FOUND_ICMP_EQ;
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
              state = LOOKING_ICMP_EQ;
            } else {
              state = LOOKING_ICMP_EQ;
            }
            break;
          }
          case NotEqual: {
            if (auto binOp = llvm::dyn_cast<llvm::BinaryOperator>(&instruction)) {
              if (binOp->getOpcode() == llvm::Instruction::Xor &&
                  instruction.getDebugLoc() == prevInst->getDebugLoc()) {
                state = FOUND_XOR;
              } else {
                state = LOOKING_ICMP_EQ;
              }
            } else {
              state = LOOKING_ICMP_EQ;
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
          state = LOOKING_ICMP_EQ;
        } else {
          state = LOOKING_ICMP_EQ;
        }
      }
      default:
        break;
    }
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
  swift::EquatableOperationFinder finder(EquatableOperationFinder::Equal);
  return finder.getMutations(bitcode, function, this);
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
  swift::EquatableOperationFinder finder(EquatableOperationFinder::NotEqual);
  return finder.getMutations(bitcode, function, this);
}

void SwiftNotEqualToEqual::applyMutation(llvm::Function *function,
                                        const mull::MutationPointAddress &address,
                                        irm::IRMutation *lowLevelMutation) {
  llvm::Instruction &inst = address.findInstruction(function);
  llvm::ICmpInst *icmp = llvm::dyn_cast<llvm::ICmpInst>(&inst);
  assert(icmp);
  icmp->setPredicate(llvm::CmpInst::ICMP_NE);
}
