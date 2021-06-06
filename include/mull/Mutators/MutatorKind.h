#pragma once

#include <functional>
#include <string>
#include <unordered_set>
#include <vector>

namespace mull {

enum class MutatorKind {
#define MUTATOR_KIND(ID) ID,
#include "mull-c/Mutators/MutatorKind.def"
#undef MUTATOR_KIND
};

std::string MutationKindToString(MutatorKind mutatorKind);

} // namespace mull

namespace std {

template <> struct hash<mull::MutatorKind> {
  std::size_t operator()(const mull::MutatorKind &k) const {
    return static_cast<std::size_t>(k);
  }
};

} // namespace std

namespace mull {
class MutatorKindSet {
public:
  static MutatorKindSet create(std::vector<MutatorKind> mutators);
  bool includesMutator(mull::MutatorKind mutatorKind) const;
private:
  MutatorKindSet(std::unordered_set<mull::MutatorKind> mutators);
  std::unordered_set<mull::MutatorKind> mutators;
};
}
