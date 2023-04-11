// RUN: not %asli --nobanner %s | %decolor | filecheck %s

func F(x : integer)
begin
    x = 1;
// CHECK: Type error: assignment to immutable variable `x` declared at
end
