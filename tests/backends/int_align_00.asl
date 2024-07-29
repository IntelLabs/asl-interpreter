// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return AlignDown(x, y);
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

// CHECK: 12
// CHECK: 12
// CHECK: 12
// CHECK: 12
// CHECK: 16
