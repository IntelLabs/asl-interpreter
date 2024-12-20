// RUN: not %asli --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

var X : integer;

func WX() => integer
begin
    X = 1;
    return 0;
end

func F() => integer
begin
    return WX() + WX();
// CHECK: Type error: expression behaviour depends on evaluation order
end
