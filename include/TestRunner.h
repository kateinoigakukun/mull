#pragma once

#include "ExecutionResult.h"

#include <llvm/Object/Binary.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/Target/TargetMachine.h>

namespace mull {

class JITEngine;
class Test;
class Instrumentation;

class TestRunner {
public:
  typedef std::vector<llvm::object::ObjectFile *> ObjectFiles;
  typedef std::vector<llvm::object::OwningBinary<llvm::object::ObjectFile>> OwnedObjectFiles;

  TestRunner();

  virtual void loadInstrumentedProgram(ObjectFiles &objectFiles, Instrumentation &instrumentation, JITEngine &jit) = 0;
  virtual void loadProgram(ObjectFiles &objectFiles, JITEngine &jit) = 0;
  virtual ExecutionStatus runTest(Test *test, JITEngine &jit) = 0;

  virtual ~TestRunner() = default;
};

}
