#include "mull/FunctionUnderTest.h"
#include "mull/MutationPoint.h"
#include "mull/Mutators/Swift/SwiftMutations.h"
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/InstVisitor.h>

using namespace mull;
using namespace mull::swift;

namespace mull::swift {
class EquatableOperationFinder {
public:
  enum Operator { Equal, NotEqual };

  EquatableOperationFinder(Operator op, mull::Bitcode *bitcode, mull::Mutator *mutator)
      : op(op), bitcode(bitcode), mutator(mutator) {}

  std::vector<MutationPoint *> getMutations(const FunctionUnderTest &function);

private:
  Operator op;

  mull::Bitcode *bitcode;
  mull::Mutator *mutator;

  // MARK: Simple operator finder
  enum PrimitiveFinderState {
    LOOKING_ICMP_EQ,
    FOUND_ICMP_EQ,
    FOUND_XOR,
  };
  PrimitiveFinderState primitiveFinderState = LOOKING_ICMP_EQ;
  llvm::Instruction *foundICMP = nullptr;
  llvm::Instruction *foundXor = nullptr;
  llvm::Instruction *prevInst = nullptr;

  // MARK: Finders

  void nextPrimitiveFinderState(const llvm::Instruction &instruction,
                                std::vector<MutationPoint *> &mutations);
  /// @return Returns a return-to BB if found.
  llvm::BasicBlock *findBinaryIntComparison(llvm::Instruction &instruction);
  mull::MutationPoint *createBinaryIntComparisonMutation(llvm::BasicBlock &returnBB);
};
} // namespace mull::swift

void EquatableOperationFinder::nextPrimitiveFinderState(const llvm::Instruction &instruction,
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

  switch (primitiveFinderState) {
  case LOOKING_ICMP_EQ: {
    if (auto icmp = llvm::dyn_cast<llvm::ICmpInst>(&instruction)) {
      if (icmp->getPredicate() == llvm::CmpInst::ICMP_EQ) {
        primitiveFinderState = FOUND_ICMP_EQ;
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
        primitiveFinderState = LOOKING_ICMP_EQ;
      } else {
        primitiveFinderState = LOOKING_ICMP_EQ;
      }
      break;
    }
    case NotEqual: {
      if (auto binOp = llvm::dyn_cast<llvm::BinaryOperator>(&instruction)) {
        if (binOp->getOpcode() == llvm::Instruction::Xor &&
            instruction.getDebugLoc() == prevInst->getDebugLoc()) {
          primitiveFinderState = FOUND_XOR;
        } else {
          primitiveFinderState = LOOKING_ICMP_EQ;
        }
      } else {
        primitiveFinderState = LOOKING_ICMP_EQ;
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
      primitiveFinderState = LOOKING_ICMP_EQ;
    } else {
      primitiveFinderState = LOOKING_ICMP_EQ;
    }
  }
  }
}

namespace {
bool isIsSignedCall(const llvm::Instruction &inst) {
  if (auto callInst = llvm::dyn_cast<llvm::CallInst>(&inst)) {
    if (auto calleeFn = callInst->getCalledFunction()) {
      llvm::StringRef calleeName = calleeFn->getName();
      return calleeName.startswith("$sSUsE8isSignedSbvgZs")
      || calleeName.startswith("$sSZsE8isSignedSbvgZs");
    }
  }
  return false;
}

bool isBitWidthCall(const llvm::Instruction &inst) {
  if (auto callInst = llvm::dyn_cast<llvm::CallInst>(&inst)) {
    if (auto calleeFn = callInst->getCalledFunction()) {
      llvm::StringRef calleeName = calleeFn->getName();
      return calleeName.startswith("$ss17FixedWidthIntegerPsE03bitB0Sivgs");
    }
  }
  return false;
}
};

llvm::BasicBlock *
EquatableOperationFinder::findBinaryIntComparison(llvm::Instruction &instruction) {
  // clang-format off
  //
  //  %4 = call swiftcc i1 @"$sSUsE8isSignedSbvgZs6UInt32V_Tgq5"(%swift.type* swiftself @"$ss6UInt32VN"), !dbg !432
  //  br i1 %4, label %5, label %6, !dbg !432
  //
  //5:                                                ; preds = %entry
  //  br label %7, !dbg !432
  //
  //6:                                                ; preds = %entry
  //  br label %7, !dbg !432
  //
  //7:                                                ; preds = %5, %6
  //  %8 = phi i1 [ false, %6 ], [ false, %5 ], !dbg !432
  //  %9 = call swiftcc i1 @"$sSUsE8isSignedSbvgZs6UInt64V_Tgq5"(%swift.type* swiftself @"$ss6UInt64VN"), !dbg !432
  //  br i1 %9, label %10, label %11, !dbg !432
  //
  //10:                                               ; preds = %7
  //  br label %12, !dbg !432
  //
  //11:                                               ; preds = %7
  //  br label %12, !dbg !432
  //
  //12:                                               ; preds = %10, %11
  //  %13 = phi i1 [ false, %11 ], [ false, %10 ], !dbg !432
  //  %14 = icmp eq i1 %8, %13, !dbg !432
  //  %15 = xor i1 %14, true, !dbg !432
  //  br i1 %15, label %16, label %17, !dbg !432
  //
  //16:                                               ; preds = %12
  //  br label %21, !dbg !432
  //
  //17:                                               ; preds = %12
  //  %18 = call swiftcc i64 @"$ss17FixedWidthIntegerPsE03bitB0Sivgs6UInt31V_Tgq5"(i32 %0), !dbg !432
  //  %19 = call swiftcc i64 @"$ss17FixedWidthIntegerPsE03bitB0Sivgs6UInt64V_Tgq5"(i64 %1), !dbg !432
  //  %20 = icmp slt i64 %18, %19, !dbg !432
  //  br i1 %20, label %23, label %28, !dbg !432
  //
  //21:                                               ; preds = %16, %26, %35, %42, %47
  //  %22 = phi i1 [ %48, %47 ], [ %43, %42 ], [ %36, %35 ], [ %27, %26 ], [ false, %16 ], !dbg !434
  //  ret i1 %22, !dbg !434
  // clang-format on

  
  std::function<bool (const llvm::CallInst &)> CallInstHandlers[] = {
    [](const llvm::CallInst &inst) {
      return isIsSignedCall(inst);
    },
    [](const llvm::CallInst &inst) {
      return isIsSignedCall(inst);
    },
    [](const llvm::CallInst &inst) {
      return isBitWidthCall(inst);
    },
    [](const llvm::CallInst &inst) {
      return isBitWidthCall(inst);
    },
  };

  std::function<bool (const llvm::BranchInst &)> BrInstHandlers[] = {
    [](const llvm::BranchInst &inst) {
      if (!inst.isConditional()) return false;
      if (!llvm::isa<llvm::CallInst>(inst.getCondition())) return false;
      return true;
    },
    [](const llvm::BranchInst &inst) { return !inst.isConditional(); },
    [](const llvm::BranchInst &inst) { return !inst.isConditional(); },

    [](const llvm::BranchInst &inst) {
      if (!inst.isConditional()) return false;
      if (!llvm::isa<llvm::CallInst>(inst.getCondition())) return false;
      return true;
    },
    [](const llvm::BranchInst &inst) { return !inst.isConditional(); },
    [](const llvm::BranchInst &inst) { return !inst.isConditional(); },

    [](const llvm::BranchInst &inst) {
      if (!inst.isConditional()) return false;
      if (auto binOp = llvm::dyn_cast<llvm::BinaryOperator>(inst.getCondition())) {
        return binOp->getOpcode() == llvm::Instruction::Xor;
      }
      return false;
    },

    [](const llvm::BranchInst &inst) { return !inst.isConditional(); },
    [](const llvm::BranchInst &inst) { return inst.isConditional(); },
  };

  unsigned InlinedOpcode[] = {
    llvm::Instruction::Call,
    llvm::Instruction::Br,
    llvm::Instruction::Br,
    llvm::Instruction::Br,
    llvm::Instruction::PHI,
    llvm::Instruction::Call,
    llvm::Instruction::Br,
    llvm::Instruction::Br,
    llvm::Instruction::Br,
    llvm::Instruction::PHI,
    llvm::Instruction::ICmp,
    llvm::Instruction::Xor,
    llvm::Instruction::Br,
    llvm::Instruction::Call,
    llvm::Instruction::Call,
    llvm::Instruction::ICmp,
    llvm::Instruction::Br,
  };

  const llvm::Instruction *currentInst = &instruction;
  const llvm::DebugLoc &debugLoc = instruction.getDebugLoc();
  uint32_t instIndex = 0;
  uint32_t callInstIndex = 0;
  uint32_t brInstIndex = 0;

  while (currentInst) {
    if (currentInst->getDebugLoc() != debugLoc) {
      return nullptr;
    }
    if (currentInst->getOpcode() != InlinedOpcode[instIndex]) {
      return nullptr;
    }
    // FIXME: Use InstVisitor
    switch (currentInst->getOpcode()) {
      case llvm::Instruction::Call: {
        const auto callInst = llvm::dyn_cast<llvm::CallInst>(currentInst);
        if (!CallInstHandlers[callInstIndex](*callInst)) {
          return nullptr;
        }
        callInstIndex += 1;
        break;
      }
      case llvm::Instruction::Br: {
        const auto brInst = llvm::dyn_cast<llvm::BranchInst>(currentInst);
        if (!BrInstHandlers[brInstIndex](*brInst)) {
          return nullptr;
        }
        brInstIndex += 1;
        break;
      }
      default:
        break;
    }
    instIndex += 1;

    if (instIndex >= sizeof(InlinedOpcode)/sizeof(unsigned)) {
      break;
    }

    if (auto nextInst = currentInst->getNextNode()) {
      currentInst = nextInst;
    } else {
      const llvm::BasicBlock *currentBB = instruction.getParent();
      if (auto nextBB = currentBB->getNextNode()) {
        currentInst = &nextBB->front();
      }
    }
  }

  if (instruction.getNextNode() != nullptr) {
    return nullptr;
  }
  llvm::BasicBlock *currentBB = instruction.getParent();
  if (auto nextBB = currentBB->getNextNode()) {
    return nextBB;
  } else {
    return nullptr;
  }
}

mull::MutationPoint *
EquatableOperationFinder::createBinaryIntComparisonMutation(llvm::BasicBlock &returnBB) {
  if (returnBB.size() < 2) {
    return nullptr;
  }
  auto instIt = returnBB.begin();
  llvm::Instruction *inst = &*instIt;
  if (!llvm::isa<llvm::PHINode>(inst)) {
    return nullptr;
  }
  switch (op) {
    case Equal: {
      break;
    }
    case NotEqual: {
      instIt++;
      inst = &*instIt;
      if (inst->getOpcode() != llvm::Instruction::Xor) {
        return nullptr;
      }
      break;
    }
  }

  return new mull::MutationPoint(mutator, nullptr, inst, bitcode);
}

std::vector<MutationPoint *>
EquatableOperationFinder::getMutations(const FunctionUnderTest &function) {
  assert(bitcode);
  std::vector<MutationPoint *> mutations;

  auto instructions = llvm::instructions(function.getFunction());

  for (auto it = instructions.begin(); it != instructions.end(); it++) {
    auto &instruction = *it;
    nextPrimitiveFinderState(instruction, mutations);
    if (auto *foundBB = findBinaryIntComparison(instruction)) {
      if (auto *point = createBinaryIntComparisonMutation(*foundBB)) {
        mutations.push_back(point);
      }
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
