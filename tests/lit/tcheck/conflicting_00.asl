// RUN: not %asli --nobanner %s | %decolor | FileCheck %s

func F();
func F();
// CHECK: Type error: function `F` was previously defined
