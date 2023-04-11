// RUN: not %asli --nobanner %s | %decolor | filecheck %s

func F()
begin
    constant x = 1;
    x = 2;
// CHECK: Type error: assignment to immutable variable `x` declared at
end
