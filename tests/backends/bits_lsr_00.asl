// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : bits(8), y : integer) => bits(8)
begin
    return lsr_bits(x, y);
end

func main() => integer
begin
    print(Test('1010 0101', 3)); println();
    print(Test('0101 1010', 2)); println();
    return 0;
end

// CHECK: 8'x14
// CHECK: 8'x16
