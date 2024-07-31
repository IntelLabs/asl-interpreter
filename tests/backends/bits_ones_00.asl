// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(N : integer) => bits(N)
begin
    return Ones(N);
end

func main() => integer
begin
    print(Test(0)); println();
// CHECK: 0'x0
    print(Test(6)); println();
// CHECK: 6'x3f
    return 0;
end

