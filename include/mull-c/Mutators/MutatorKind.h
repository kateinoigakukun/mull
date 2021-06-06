#pragma once

#ifdef __cplusplus
extern "C" {
#endif

enum CMutatorKind {
#define MUTATOR_KIND(ID) ID,
#include "mull-c/Mutators/MutatorKind.def"
#undef MUTATOR_KIND
};

#ifdef __cplusplus
}
#endif
