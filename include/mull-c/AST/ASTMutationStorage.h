#pragma once

#include "mull-c/Mutators/MutatorKind.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef void *CASTMutationStorage;

void mull_ASTMutationStorage_saveMutation(CASTMutationStorage,
                                          const char *sourceFile,
                                          enum CMutatorKind mutatorKind,
                                          int line, int column);

#ifdef __cplusplus
}
#endif
