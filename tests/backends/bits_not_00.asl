// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : bits(8)) => bits(8)
begin
    return (NOT x);
end

func main() => integer
begin
    print(Test('1011 0011')); println();
    return 0;
end

// CHECK: 8'x4c
