// RUN: not %asli --nobanner %s | %decolor | filecheck %s

func F{N}(x : bits(N))
begin
    N = 1;
// CHECK: Type error: assignment to immutable variable `N` declared at
end
