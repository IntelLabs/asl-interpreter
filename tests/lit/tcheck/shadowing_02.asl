// RUN: not %asli --nobanner %s | %decolor | FileCheck %s

func F{N}(x : bits(N))
begin
    let N = 32;
// CHECK: Type error: variable `N` previously declared
end
