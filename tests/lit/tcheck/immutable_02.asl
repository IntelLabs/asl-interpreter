// RUN: not %asli --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func F{N}(x : bits(N))
begin
    N = 1;
// CHECK: Type error: assignment to immutable variable `N` declared at
end
