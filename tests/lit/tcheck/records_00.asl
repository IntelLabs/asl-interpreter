// RUN: not %asli --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

record R {
    x : integer;
    x : integer;
// CHECK: Type error: fieldname `x` is declared multiple times
};
