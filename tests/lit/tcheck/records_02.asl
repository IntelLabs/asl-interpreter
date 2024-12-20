// RUN: not %asli --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

record R {
    x : integer;
    y : integer;
};

func F() => R
begin
    return R{x = 1};
// CHECK: Type error: record initializer is missing fields y and/or has extra fields
end
