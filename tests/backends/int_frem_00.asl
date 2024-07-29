// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return frem_int(x, y);
end

func main() => integer
begin
    print_int_dec(Test(6, 3)); println();
    print_int_dec(Test(-6, 3)); println();
    print_int_dec(Test(5, 3)); println();
    print_int_dec(Test(-5, 3)); println();
    print_int_dec(Test(6, -3)); println();
    return 0;
end

// CHECK: 0
// CHECK: 0
// CHECK: 2
// CHECK: 1
// CHECK: 0
