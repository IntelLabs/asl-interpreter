// RUN: not %asli --nobanner %s | %decolor | filecheck %s

func F();
func F();
// CHECK: Type error: function `F` was previously defined
