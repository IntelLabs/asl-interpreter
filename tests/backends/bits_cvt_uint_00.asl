// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : bits(4)) => integer {0..15}
begin
    return UInt(x);
end

func main() => integer
begin
    print_int_dec(Test('0010')); println();
    print_int_dec(Test('1010')); println();
    return 0;
end

// CHECK: 2
// CHECK: 10
