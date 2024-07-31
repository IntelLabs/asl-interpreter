// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : integer, N : integer) => bits(N)
begin
    return cvt_int_bits(x, N);
end

func main() => integer
begin
    print(Test(42, 8)); println();
    // CHECK: 8'x2a
    print(Test(-1, 8)); println();
    // CHECK: 8'xff
    return 0;
end

