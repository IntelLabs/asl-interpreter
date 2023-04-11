// RUN: not %asli --nobanner %s | %decolor | filecheck %s

func F(x : integer)
begin
    let x = 42;
// CHECK: Type error: variable `x` previously declared
end
