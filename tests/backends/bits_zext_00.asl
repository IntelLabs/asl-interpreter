// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : bits(M), N : integer) => bits(N)
begin
    return ZeroExtend(x, N);
end

func main() => integer
begin
    print(Test('110', 8)); println();
    // CHECK: 8'x6
    print(Test('011', 8)); println();
    // CHECK: 8'x3
    return 0;
end

