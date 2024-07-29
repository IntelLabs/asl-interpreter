// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return x * y;
end

func main() => integer
begin
    print_int_dec(Test(4, 5)); println();
    print_int_dec(Test(0, 2)); println();
    print_int_dec(Test(3, -2)); println();
    return 0;
end

// CHECK: 20
// CHECK: 0
// CHECK: -6
