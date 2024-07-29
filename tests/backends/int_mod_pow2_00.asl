// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return mod_pow2_int(x, y);
end

func main() => integer
begin
    print_int_dec(Test(12, 2)); println();
    print_int_dec(Test(13, 2)); println();
    print_int_dec(Test(14, 2)); println();
    print_int_dec(Test(15, 2)); println();
    print_int_dec(Test(16, 2)); println();
    return 0;
end

// CHECK: 0
// CHECK: 1
// CHECK: 2
// CHECK: 3
// CHECK: 0
