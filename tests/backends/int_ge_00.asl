// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : integer, y : integer) => boolean
begin
    return x >= y;
end

func main() => integer
begin
    print(Test(1, 2)); println();
    // CHECK: FALSE
    print(Test(1, 1)); println();
    // CHECK: TRUE
    print(Test(1, 0)); println();
    // CHECK: TRUE
    print(Test(-1, 0)); println();
    // CHECK: FALSE
    return 0;
end
