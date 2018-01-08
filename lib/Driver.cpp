#include "Driver.h"

#include "Config.h"
#include "Context.h"
#include "Logger.h"
#include "ModuleLoader.h"
#include "Result.h"
#include "Testee.h"
#include "MutationResult.h"
#include "TestFinder.h"
#include "TestRunner.h"
#include "MutationsFinder.h"

#include <llvm/Support/DynamicLibrary.h>

#include <algorithm>
#include <fstream>
#include <vector>
#include <sys/mman.h>
#include <sys/types.h>

using namespace llvm;
using namespace llvm::object;
using namespace mull;
using namespace std;

Driver::~Driver() {
  delete this->Sandbox;
  delete this->diagnostics;
  munmap(_callTreeMapping, functions.size());
}

/// Populate mull::Context with modules using
/// ModulePaths from mull::Config.
/// mull::Context should be populated using ModuleLoader
/// so that we could inject modules from string for testing purposes

/// Having mull::Context in place we could instantiate TestFinder and find all tests
/// Using same TestFinder we could find mutation points, apply them sequentially and
/// run tests/mutants using newly created TestRunner

/// This method should return (somehow) results of the tests/mutants execution
/// So that we could easily plug in some TestReporter

/// UPD: The method returns set of results
/// Number of results equals to a number of tests
/// Each result contains result of execution of an original test and
/// all the results of each mutant within corresponding MutationPoint

std::unique_ptr<Result> Driver::Run() {
  /// Assumption: all modules will be used during the execution
  /// Therefore we load them into memory and compile immediately
  /// Later on modules used only for generating of mutants
  std::vector<std::string> bitcodePaths = Cfg.getBitcodePaths();
  std::vector<unique_ptr<MullModule>> modules =
    Loader.loadModulesFromBitcodeFileList(bitcodePaths);

  for (auto &ownedModule : modules) {
    MullModule &module = *ownedModule.get();
    assert(ownedModule && "Can't load module");
    Ctx.addModule(std::move(ownedModule));

    ObjectFile *objectFile = toolchain.cache().getObject(module);

    if (objectFile == nullptr) {
      LLVMContext localContext;

      auto clonedModule = module.clone(localContext);

      for (auto &function: module.getModule()->getFunctionList()) {
        if (function.isDeclaration()) {
          continue;
        }
        CallTreeFunction callTreeFunction(&function);
        uint64_t index = functions.size();
        functions.push_back(callTreeFunction);
        auto clonedFunction = clonedModule->getModule()->getFunction(function.getName());
        callbacks.injectCallbacks(clonedFunction, index, this);
      }

      auto owningObjectFile = toolchain.compiler().compileModule(*clonedModule.get());
      objectFile = owningObjectFile.getBinary();
      toolchain.cache().putObject(std::move(owningObjectFile), module);
    }

    InnerCache.insert(std::make_pair(module.getModule(), objectFile));
  }

  for (std::string &objectFilePath: Cfg.getObjectFilesPaths()) {
    ErrorOr<std::unique_ptr<MemoryBuffer>> buffer =
      MemoryBuffer::getFile(objectFilePath.c_str());

    if (!buffer) {
      Logger::error() << "Cannot load object file: " << objectFilePath << "\n";
      continue;
    }

    Expected<std::unique_ptr<ObjectFile>> objectOrError =
      ObjectFile::createObjectFile(buffer.get()->getMemBufferRef());

    if (!objectOrError) {
      Logger::error() << "Cannot create object file: " << objectFilePath << "\n";
      continue;
    }

    std::unique_ptr<ObjectFile> objectFile(std::move(objectOrError.get()));

    auto owningObject = OwningBinary<ObjectFile>(std::move(objectFile),
                                                 std::move(buffer.get()));
    precompiledObjectFiles.push_back(std::move(owningObject));
  }

  prepareForExecution();

  auto foundTests = Finder.findTests(Ctx, filter);
  const int testsCount = foundTests.size();

  Logger::debug() << "Driver::Run> found "
                  << testsCount
                  << " tests\n";

  for (std::string &dylibPath: Cfg.getDynamicLibrariesPaths()) {
    sys::DynamicLibrary::LoadLibraryPermanently(dylibPath.c_str());
  }

  Logger::debug() << "Driver::Run> running tests and searching mutations\n";

  std::vector<MutationPoint *> allMutationPoints;
  auto objectFiles = AllObjectFiles();
  auto testIndex = 1;
  for (auto &test : foundTests) {
    Logger::debug().indent(2) << "[" << testIndex++ << "/" << testsCount << "] " << test->getTestDisplayName() << ": ";

    _callstack = stack<uint64_t>();
    memset(_callTreeMapping, 0, functions.size() * sizeof(_callTreeMapping[0]));

    ExecutionResult testExecutionResult = Sandbox->run([&]() {
      return Runner.runTest(test.get(), objectFiles);
    }, Cfg.getTimeout());

    Logger::debug() << testExecutionResult.getStatusAsString() << "\n";

    test->setExecutionResult(testExecutionResult);

    if (testExecutionResult.status != Passed) {
      continue;
    }

    std::unique_ptr<CallTree> callTree(dynamicCallTree.createCallTree());

    auto subtrees = dynamicCallTree.extractTestSubtrees(callTree.get(), test.get());
    auto testees = dynamicCallTree.createTestees(subtrees, test.get(),
                                                 Cfg.getMaxDistance(),
                                                 filter);

    dynamicCallTree.cleanupCallTree(std::move(callTree));
    if (testees.empty()) {
      continue;
    }

    for (auto testee_it = std::next(testees.begin()), ee = testees.end();
         testee_it != ee;
         ++testee_it) {

      std::unique_ptr<Testee> &testee = *testee_it;

      auto mutationPoints = mutationsFinder.getMutationPoints(Ctx, *testee.get(), filter);
      std::copy(mutationPoints.begin(), mutationPoints.end(), std::back_inserter(allMutationPoints));
    }
  }

  Logger::debug() << "Driver::Run> found " << allMutationPoints.size() << " mutations\n";

  std::vector<std::unique_ptr<MutationResult>> mutationResults;
  if (Cfg.isDryRun()) {
    mutationResults = dryRunMutations(allMutationPoints);
  } else {
    mutationResults = runMutations(allMutationPoints);
  }

  return make_unique<Result>(std::move(foundTests), std::move(mutationResults), allMutationPoints);
}

std::vector<std::unique_ptr<MutationResult>> Driver::dryRunMutations(const std::vector<MutationPoint *> &mutationPoints) {
  std::vector<std::unique_ptr<MutationResult>> mutationResults;

  const auto mutationsCount = mutationPoints.size();
  auto mutantIndex = 1;

  for (auto mutationPoint : mutationPoints) {
    Logger::debug() << "[" << mutantIndex++ << "/" << mutationsCount << "]: "  << mutationPoint->getUniqueIdentifier() << "\n";

    auto testsCount = mutationPoint->getReachableTests().size();
    auto testIndex = 1;

    for (auto &reachableTest : mutationPoint->getReachableTests()) {
      auto test = reachableTest.first;
      auto distance = reachableTest.second;

      Logger::debug().indent(2) << "[" << testIndex++ << "/" << testsCount << "] " << test->getTestDisplayName() << ": ";

      auto timeout = test->getExecutionResult().runningTime * 10;

      ExecutionResult result;
      result.status = DryRun;
      result.runningTime = timeout;

      Logger::debug() << result.getStatusAsString() << "\n";

      mutationResults.push_back(make_unique<MutationResult>(result, mutationPoint, distance, test));
    }
  }

  return mutationResults;
}

std::vector<std::unique_ptr<MutationResult>> Driver::runMutations(const std::vector<MutationPoint *> &mutationPoints) {
  std::vector<std::unique_ptr<MutationResult>> mutationResults;

  const auto failFastEnabled = false;

  const auto mutationsCount = mutationPoints.size();
  auto mutantIndex = 1;

  for (auto mutationPoint : mutationPoints) {
    auto objectFilesWithMutant = AllButOne(mutationPoint->getOriginalModule()->getModule());

    Logger::debug() << "[" << mutantIndex++ << "/" << mutationsCount << "]: "  << mutationPoint->getUniqueIdentifier() << "\n";

    LLVMContext localContext;
    auto clonedModule = mutationPoint->getOriginalModule()->clone(localContext);
    mutationPoint->applyMutation(*clonedModule.get());
    auto ownedMutant = toolchain.compiler().compileModule(*clonedModule.get());
    ObjectFile *mutant = ownedMutant.getBinary();

    objectFilesWithMutant.push_back(mutant);

    auto testsCount = mutationPoint->getReachableTests().size();
    auto testIndex = 1;

    auto alreadyFailed = false;

    for (auto &reachableTest : mutationPoint->getReachableTests()) {
      auto test = reachableTest.first;
      auto distance = reachableTest.second;

      Logger::debug().indent(2) << "[" << testIndex++ << "/" << testsCount << "] " << test->getTestDisplayName() << ": ";

      ExecutionResult result;

      if (failFastEnabled && alreadyFailed) {
        result.status = ExecutionStatus::FailFast;
      } else {
        const auto timeout = test->getExecutionResult().runningTime * 10;
        const auto sandboxTimeout = std::max(30LL, timeout);

        result = Sandbox->run([&]() {
          ExecutionStatus status = Runner.runTest(test, objectFilesWithMutant);
          assert(status != ExecutionStatus::Invalid && "Expect to see valid TestResult");
          return status;
        }, sandboxTimeout);

        assert(result.status != ExecutionStatus::Invalid &&
               "Expect to see valid TestResult");

        if (result.status != ExecutionStatus::Passed) {
          alreadyFailed = true;
        }
      }

      Logger::debug() << result.getStatusAsString() << "\n";

      mutationResults.push_back(make_unique<MutationResult>(result, mutationPoint, distance, test));
    }

    objectFilesWithMutant.pop_back();
  }

  return mutationResults;
}

void Driver::prepareForExecution() {
  assert(_callTreeMapping == nullptr && "Called twice?");
  assert(functions.size() > 1 && "Functions must be filled in before this call");

  /// Creating a memory to be shared between child and parent.
  _callTreeMapping = (uint64_t *) mmap(NULL,
                                       sizeof(uint64_t) * functions.size(),
                                       PROT_READ | PROT_WRITE,
                                       MAP_SHARED | MAP_ANONYMOUS,
                                       -1,
                                       0);
  memset(_callTreeMapping, 0, functions.size() * sizeof(_callTreeMapping[0]));
  dynamicCallTree.prepare(_callTreeMapping);
}

std::vector<llvm::object::ObjectFile *> Driver::AllButOne(llvm::Module *One) {
  std::vector<llvm::object::ObjectFile *> Objects;

  for (auto &CachedEntry : InnerCache) {
    if (One != CachedEntry.first) {
      Objects.push_back(CachedEntry.second);
    }
  }

  for (OwningBinary<ObjectFile> &object: precompiledObjectFiles) {
    Objects.push_back(object.getBinary());
  }

  return Objects;
}

std::vector<llvm::object::ObjectFile *> Driver::AllObjectFiles() {
  std::vector<llvm::object::ObjectFile *> Objects;

  for (auto &CachedEntry : InnerCache) {
    Objects.push_back(CachedEntry.second);
  }

  for (OwningBinary<ObjectFile> &object: precompiledObjectFiles) {
    Objects.push_back(object.getBinary());
  }

  return Objects;
}
