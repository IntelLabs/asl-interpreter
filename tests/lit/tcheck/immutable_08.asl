// RUN: not %asli --nobanner %s | %decolor | FileCheck %s

constant C : integer = 1;

func F(x : integer)
begin
    C = 2;
// CHECK: Type error: assignment to immutable variable `C` declared at
end
