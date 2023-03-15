// RUN: not %asli --nobanner %s | %decolor | FileCheck %s

func F() => bits(N);
// CHECK: Type error: the width parameter(s) `N` of the return type cannot be determined from the function arguments
