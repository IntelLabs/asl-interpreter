// RUN: not %asli --nobanner %s | %decolor | filecheck %s

func F{N}(x : bits(N))
begin
    let N = 32;
// CHECK: Type error: variable `N` previously declared
end
