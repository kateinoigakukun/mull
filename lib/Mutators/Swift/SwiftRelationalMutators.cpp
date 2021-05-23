#include "mull/FunctionUnderTest.h"
#include "mull/MutationPoint.h"
#include "mull/Mutators/Swift/SwiftMutations.h"
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/InstVisitor.h>

using namespace mull;
using namespace mull::swift;

namespace mull::swift {
enum class EquatableOperator { Equal, NotEqual };

class EquatableOperationFinder {
public:

  EquatableOperationFinder(EquatableOperator op, mull::Bitcode *bitcode, mull::Mutator *mutator)
      : op(op), bitcode(bitcode), mutator(mutator) {}

  std::vector<MutationPoint *> getMutations(const FunctionUnderTest &function);

private:
  EquatableOperator op;

  mull::Bitcode *bitcode;
  mull::Mutator *mutator;

  // MARK: Simple operator finder
  enum PrimitiveFinderState {
    LOOKING_EQ_INST,
    FOUND_EQ_INST,
    FOUND_XOR,
  };
  PrimitiveFinderState primitiveFinderState = LOOKING_EQ_INST;
  llvm::Instruction *foundEqInst = nullptr;
  llvm::Instruction *foundXor = nullptr;
  llvm::Instruction *prevInst = nullptr;

  // MARK: Finders

  void nextPrimitiveFinderState(const llvm::Instruction &instruction,
                                std::vector<MutationPoint *> &mutations);
};

class BinaryIntegerPatternFinder {
  EquatableOperator op;
public:
  BinaryIntegerPatternFinder(EquatableOperator op) : op(op) {}
  /// @return Returns a return-to BB if found.
  llvm::BasicBlock *findReturnToBB(llvm::Instruction &instruction);
  llvm::Instruction *findResultInst(llvm::BasicBlock &returnBB);
};

} // namespace mull::swift


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

bool isEquatableEqCall(const llvm::Instruction &inst) {
  if (auto callInst = llvm::dyn_cast<llvm::CallInst>(&inst)) {
    if (auto calleeFn = callInst->getCalledFunction()) {
      llvm::StringRef calleeName = calleeFn->getName();
      return calleeName.startswith("$sSQ2eeoiySbx_xtFZTj");
    }
  }
  return false;
}
};

void EquatableOperationFinder::nextPrimitiveFinderState(const llvm::Instruction &instruction,
                                                        std::vector<MutationPoint *> &mutations) {
  //     For (op == Equal)
  //
  //     /----> LOOKING_EQ_INST -----\
  //     |                           | (icmp eq or Equatable.==)
  //     |                           v
  // FOUND_EQ <----------------- FOUND_EQ_INST
  //          (different dbg node)
  //
  //
  //     For (op == NotEqual)
  //
  //          (icmp eq or Equatable.==)
  // LOOKING_EQ_INST --------> FOUND_EQ_INST
  //     ^                           |
  //     |                           |
  //     |                           v
  // FOUND_NEQ <----------------- FOUND_XOR
  //          (different dbg node)
  //

  switch (primitiveFinderState) {
  case LOOKING_EQ_INST: {
    if (auto icmp = llvm::dyn_cast<llvm::ICmpInst>(&instruction)) {
      if (icmp->getPredicate() == llvm::CmpInst::ICMP_EQ) {
        primitiveFinderState = FOUND_EQ_INST;
      }
    } else if (isEquatableEqCall(instruction)) {
      primitiveFinderState = FOUND_EQ_INST;
    }
    break;
  }
  case FOUND_EQ_INST: {
    assert(prevInst);
    foundEqInst = prevInst;
    switch (op) {
    case EquatableOperator::Equal: {
      if (instruction.getDebugLoc() != prevInst->getDebugLoc()) {
        mutations.push_back(new mull::MutationPoint(mutator, nullptr, foundEqInst, bitcode));
        primitiveFinderState = LOOKING_EQ_INST;
      } else {
        primitiveFinderState = LOOKING_EQ_INST;
      }
      break;
    }
    case EquatableOperator::NotEqual: {
      if (auto binOp = llvm::dyn_cast<llvm::BinaryOperator>(&instruction)) {
        if (binOp->getOpcode() == llvm::Instruction::Xor &&
            instruction.getDebugLoc() == prevInst->getDebugLoc()) {
          primitiveFinderState = FOUND_XOR;
        } else {
          primitiveFinderState = LOOKING_EQ_INST;
        }
      } else {
        primitiveFinderState = LOOKING_EQ_INST;
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
      mutations.push_back(new mull::MutationPoint(mutator, nullptr, foundEqInst, bitcode));
      primitiveFinderState = LOOKING_EQ_INST;
    } else {
      primitiveFinderState = LOOKING_EQ_INST;
    }
  }
  }
}

void dumpLLVM(const llvm::Value *value) {
  value->print(llvm::outs());
}

llvm::BasicBlock *
BinaryIntegerPatternFinder::findReturnToBB(llvm::Instruction &instruction) {
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

  struct InlinedOpEntry {
    unsigned opcode;
    bool isOptional;
  };
  InlinedOpEntry InlinedOpcode[] = {
    { llvm::Instruction::Call, false },
    { llvm::Instruction::Br,   false },

    { llvm::Instruction::ICmp, true  },
    { llvm::Instruction::Br,   false },

    { llvm::Instruction::Br,   false },

    { llvm::Instruction::PHI,  false },
    { llvm::Instruction::Call, false },
    { llvm::Instruction::Br,   false },

    { llvm::Instruction::ICmp, true  },
    { llvm::Instruction::Br,   false },

    { llvm::Instruction::Br,   false },

    { llvm::Instruction::PHI,  false },
    { llvm::Instruction::ICmp, false },
    { llvm::Instruction::Xor,  false },
    { llvm::Instruction::Br,   false },
    { llvm::Instruction::Br,   false },
    { llvm::Instruction::Call, false },
    { llvm::Instruction::Call, false },
    { llvm::Instruction::ICmp, false },
    { llvm::Instruction::Br,   false },
  };

  llvm::Instruction *currentInst = &instruction;
  const llvm::DebugLoc &debugLoc = instruction.getDebugLoc();
  uint32_t instIndex = 0;
  uint32_t callInstIndex = 0;
  uint32_t brInstIndex = 0;

  while (currentInst) {
    if (currentInst->getDebugLoc() != debugLoc) {
      return nullptr;
    }
    auto expected = InlinedOpcode[instIndex];
    if (currentInst->getOpcode() != expected.opcode) {
      if (!expected.isOptional) {
        return nullptr;
      } else {
        instIndex += 1;
        continue;
      }
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

    if (instIndex >= sizeof(InlinedOpcode)/sizeof(InlinedOpEntry)) {
      break;
    }

    if (auto nextInst = currentInst->getNextNode()) {
      currentInst = nextInst;
    } else {
      llvm::BasicBlock *currentBB = currentInst->getParent();
      if (auto nextBB = currentBB->getNextNode()) {
        currentInst = &nextBB->front();
      }
    }
  }

  if (currentInst->getNextNode() != nullptr) {
    return nullptr;
  }
  llvm::BasicBlock *currentBB = currentInst->getParent();
  if (auto nextBB = currentBB->getNextNode()) {
    return nextBB;
  } else {
    return nullptr;
  }
}

llvm::Instruction *
BinaryIntegerPatternFinder::findResultInst(llvm::BasicBlock &returnBB) {
  if (returnBB.size() < 2) {
    return nullptr;
  }
  auto instIt = returnBB.begin();
  llvm::Instruction *inst = &*instIt;
  if (!llvm::isa<llvm::PHINode>(inst)) {
    return nullptr;
  }
  switch (op) {
    case EquatableOperator::Equal: {
      break;
    }
    case EquatableOperator::NotEqual: {
      instIt++;
      inst = &*instIt;
      if (inst->getOpcode() != llvm::Instruction::Xor) {
        return nullptr;
      }
      break;
    }
  }
  return inst;
}

std::vector<MutationPoint *>
EquatableOperationFinder::getMutations(const FunctionUnderTest &function) {
  assert(bitcode);
  std::vector<MutationPoint *> mutations;
  BinaryIntegerPatternFinder finder(op);

  auto instructions = llvm::instructions(function.getFunction());

  for (auto it = instructions.begin(); it != instructions.end(); it++) {
    auto &instruction = *it;
    nextPrimitiveFinderState(instruction, mutations);

    if (finder.findReturnToBB(instruction) != nullptr) {
      auto *point = new mull::MutationPoint(mutator, nullptr, &instruction, bitcode);
      mutations.push_back(point);
    }

    prevInst = &instruction;
  }
  return mutations;
}

static void mutateEquatableOperation(EquatableOperator originalOp, llvm::Instruction &inst) {
  llvm::Instruction *resultInst;
  swift::BinaryIntegerPatternFinder finder(originalOp);
  if (auto returnBB = finder.findReturnToBB(inst)) {
    resultInst = finder.findResultInst(*returnBB);
  } else {
    resultInst = &inst;
  }
  auto *clonedResultInst = resultInst->clone();
  auto *notInstruction = llvm::BinaryOperator::CreateNot(clonedResultInst, "not");
  resultInst->replaceAllUsesWith(notInstruction);
  clonedResultInst->insertAfter(resultInst);
  notInstruction->insertAfter(clonedResultInst);
  resultInst->eraseFromParent();
}

std::string SwiftEqualToNotEqual::ID() {
  return "swift_eq_to_ne";
}

std::string SwiftEqualToNotEqual::description() {
  return "Replaces == with !=";
}

std::vector<MutationPoint *> SwiftEqualToNotEqual::getMutations(Bitcode *bitcode,
                                                                const FunctionUnderTest &function) {
  swift::EquatableOperationFinder finder(EquatableOperator::Equal, bitcode, this);
  return finder.getMutations(function);
}

void SwiftEqualToNotEqual::applyMutation(llvm::Function *function,
                                         const mull::MutationPointAddress &address,
                                         irm::IRMutation *lowLevelMutation) {
  llvm::Instruction &inst = address.findInstruction(function);
  mutateEquatableOperation(EquatableOperator::Equal, inst);
}

std::string SwiftNotEqualToEqual::ID() {
  return "swift_ne_to_eq";
}

std::string SwiftNotEqualToEqual::description() {
  return "Replaces != with ==";
}

std::vector<MutationPoint *> SwiftNotEqualToEqual::getMutations(Bitcode *bitcode,
                                                                const FunctionUnderTest &function) {
  swift::EquatableOperationFinder finder(EquatableOperator::NotEqual, bitcode, this);
  return finder.getMutations(function);
}

void SwiftNotEqualToEqual::applyMutation(llvm::Function *function,
                                         const mull::MutationPointAddress &address,
                                         irm::IRMutation *lowLevelMutation) {
  llvm::Instruction &inst = address.findInstruction(function);
  mutateEquatableOperation(EquatableOperator::NotEqual, inst);
}
