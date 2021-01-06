int bitwise_xor_assign(int a, int b) {
  int c = a;
  c ^= b;
  return c;
}

int main() {
  return ! (bitwise_xor_assign(1, 1) == 0);
}

// clang-format off

/**
RUN: cd / && %CLANG_EXEC -fembed-bitcode -g %s -o %s.exe
RUN: cd %CURRENT_DIR
RUN: sed -e "s:%PWD:%S:g" %S/compile_commands.json.template > %S/compile_commands.json
RUN: (unset TERM; %MULL_EXEC -linker=%clang_cxx -workers=1 -debug -enable-ast -enable-junk-detection -mutators=cxx_xor_assign_to_or_assign -reporters=IDE -ide-reporter-show-killed -compdb-path %S/compile_commands.json %s.exe 2>&1; test $? = 0) | %FILECHECK_EXEC %s --dump-input=fail --strict-whitespace --match-full-lines
CHECK-NOT:{{^.*[Ee]rror.*$}}
CHECK-NOT:{{^.*[Ww]arning.*$}}

CHECK:[info] AST Search: looking for mutations in the source files (threads: 1)
CHECK:[debug] AST Search: recording mutation "Bitwise Xor-Assign to Or-Assign": {{.*}}sample.cpp:3:5

CHECK:[info] Applying filter: AST mutation filter (threads: 1)
CHECK:[debug] ASTMutationFilter: whitelisting mutation "Bitwise Xor-Assign to Or-Assign": {{.*}}sample.cpp:3:5

CHECK:[info] Applying filter: junk (threads: 1)
CHECK:[debug] ASTMutationStorage: recording mutation "Bitwise Xor-Assign to Or-Assign": {{.*}}sample.cpp:3:5

CHECK:[info] Killed mutants (1/1):
CHECK:{{^.*}}sample.cpp:3:5: warning: Killed: Replaced ^= with |= [cxx_xor_assign_to_or_assign]{{$}}
CHECK:  c ^= b;
CHECK:    ^
CHECK:[info] Mutation score: 100%
CHECK:[info] Total execution time: {{.*}}
CHECK-EMPTY:
**/
