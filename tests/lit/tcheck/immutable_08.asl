// RUN: not %asli --nobanner %s | %decolor | filecheck %s

constant C : integer = 1;

func F()
begin
    C = 2;
// CHECK: Type error: assignment to immutable variable `C` declared at
end
