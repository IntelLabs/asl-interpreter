// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : bits(4)) => integer {-8..7}
begin
    return SInt(x);
end

func main() => integer
begin
    print_int_dec(Test('0010')); println();
    print_int_dec(Test('1010')); println();
    return 0;
end

// CHECK: 2
// CHECK: -6
