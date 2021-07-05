
#include "mull/FunctionUnderTest.h"
#include "mull/MutationPoint.h"
#include "mull/Mutators/Swift/SwiftMutations.h"
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>

using namespace mull;
using namespace mull::swift;

std::string SwiftLogicalAndToOr::ID() {
  return "swift_logical_and_to_or";
}

std::string SwiftLogicalAndToOr::description() {
  return "Replaces && with ||";
}

// entry:
//   br i1 %lhs, label %ifTrueBB, label %ifFalseBB
//
// ifTrueBB:
//   br label %phiBB
//
// ifFalseBB:
//   br label %phiBB
//
// phiBB:
//   %result = phi i1 [ false, %ifFalseBB ], [ %rhs, %ifTrueBB ]

static llvm::PHINode *findPossiblePhi(llvm::Instruction &inst, llvm::BasicBlock **ifTrueBB,
                                      llvm::BasicBlock **ifFalseBB) {
  using namespace llvm;
  auto *branchInst = llvm::dyn_cast<BranchInst>(&inst);
  if (branchInst == nullptr) {
    return nullptr;
  }

  if (!branchInst->isConditional()) {
    return nullptr;
  }

  auto *ifTrueCandBB = dyn_cast<BasicBlock>(branchInst->getOperand(2));
  auto *ifFalseCandBB = dyn_cast<BasicBlock>(branchInst->getOperand(1));

  // FIXME: やっぱり1じゃないケースがある。
  //     if self.blockMode.options.contains(.paddingRequired) && block.count != AES.blockSize {
  if (!(ifTrueCandBB->size() == 1 && ifFalseCandBB->size() == 1)) {
    return nullptr;
  }
  auto *trueBranch = dyn_cast<BranchInst>(&ifTrueCandBB->front());
  auto *falseBranch = dyn_cast<BranchInst>(&ifFalseCandBB->front());
  if (trueBranch == nullptr || falseBranch == nullptr) {
    return nullptr;
  }
  if (!trueBranch->isUnconditional() || !falseBranch->isUnconditional()) {
    return nullptr;
  }

  if (trueBranch->getOperand(0) != falseBranch->getOperand(0)) {
    return nullptr;
  }

  auto *phiDestBB = dyn_cast<BasicBlock>(trueBranch->getOperand(0));
  if (phiDestBB->empty()) {
    return nullptr;
  }

  auto *phiNode = dyn_cast<PHINode>(&phiDestBB->front());
  if (!phiNode) {
    return nullptr;
  }

  *ifTrueBB = ifTrueCandBB;
  *ifFalseBB = ifFalseCandBB;
  return phiNode;
}

static bool findPossibleMutation(llvm::Instruction &inst) {
  using namespace llvm;
  BasicBlock *ifTrueBB;
  BasicBlock *ifFalseBB;
  PHINode *phiNode = findPossiblePhi(inst, &ifTrueBB, &ifFalseBB);
  if (!phiNode || phiNode->getNumIncomingValues() != 2) {
    return false;
  }

  for (unsigned i = 0, e = phiNode->getNumIncomingValues(); i < e; ++i) {
    Value *incoming = phiNode->getIncomingValue(i);
    BasicBlock *fromBB = phiNode->getIncomingBlock(i);
    if (fromBB == ifFalseBB) {
      auto *constFalse = dyn_cast<ConstantInt>(incoming);
      if (!constFalse)
        return false;

      if (constFalse->getType() != llvm::Type::getInt1Ty(inst.getContext()) ||
          constFalse->getValue().getBoolValue() != false) {
        return false;
      }
    } else if (fromBB == ifTrueBB) {
      continue;
    } else {
      return false;
    }
  }
  return true;
}

std::vector<MutationPoint *> SwiftLogicalAndToOr::getMutations(Bitcode *bitcode,
                                                               const FunctionUnderTest &function) {
  assert(bitcode);
  std::vector<MutationPoint *> mutations;

  for (auto &instruction : llvm::instructions(function.getFunction())) {
    if (findPossibleMutation(instruction)) {
      auto point =
          new mull::MutationPoint(this, nullptr, &instruction, bitcode);
      mutations.push_back(point);
    }
  }
  return mutations;
}

void SwiftLogicalAndToOr::applyMutation(llvm::Function *function,
                                        const mull::MutationPointAddress &address,
                                        irm::IRMutation *lowLevelMutation) {
  llvm::Instruction &inst = address.findInstruction(function);
  llvm::BasicBlock *ifTrueBB;
  llvm::BasicBlock *ifFalseBB;
  llvm::PHINode *phiNode = findPossiblePhi(inst, &ifTrueBB, &ifFalseBB);
  // Before: phi i1 [ false, %falseBB ], [ %rhs, %trueBB ]
  // After:  phi i1 [ %rhs, %falseBB ],  [ true, %trueBB ]
  llvm::Value *rhs = phiNode->getIncomingValue(1);
  phiNode->setIncomingValue(0, rhs);
  phiNode->setIncomingValue(1, llvm::ConstantInt::getFalse(function->getContext()));
}
