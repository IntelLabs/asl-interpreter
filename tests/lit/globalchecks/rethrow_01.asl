// RUN: not %asli --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

type E of exception;

func T() => integer
begin
    throw E;
end

func G() => integer
begin
    return T()? + 1;
end

func F() => integer
begin
    return G();
// CHECK: Type error: call to function `G.0` should be marked with `?` because it can throw an exception
end

