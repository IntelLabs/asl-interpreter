// RUN: not %asli --nobanner %s | %decolor | FileCheck %s

func F(x : integer)
begin
    var x = 42;
// CHECK: Type error: variable `x` previously declared
end
