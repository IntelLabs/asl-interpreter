// RUN: not %asli --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

setter A[] = value : integer;

func T()
begin
    A[] = TRUE;
// CHECK: Type error: type integer does not match boolean
end
