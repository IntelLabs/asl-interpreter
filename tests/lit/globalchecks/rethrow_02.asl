// RUN: %asli --nobanner %s
// Copyright (C) 2023-2024 Intel Corporation

type E of exception;

func T() => integer
begin
    throw E;
end

func F() => integer
begin
    return T()?;
end

