// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : bits(4), y : bits(4)) => bits(4)
begin
    return x OR y;
end

func main() => integer
begin
    print_bits_hex(Test('1100', '1010')); println();
    // CHECK: 4'xe
    return 0;
end
