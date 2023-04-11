// RUN: not %asli --nobanner %s | %decolor | filecheck %s

func F()
begin
    let x = 1;
    x = 2;
// CHECK: Type error: assignment to immutable variable `x` declared at
end
