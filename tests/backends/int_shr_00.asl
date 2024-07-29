// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return x >> y;
end

func main() => integer
begin
    print_int_dec(Test(27, 0)); println();
    print_int_dec(Test(27, 1)); println();
    print_int_dec(Test(27, 2)); println();
    return 0;
end

// CHECK: 27
// CHECK: 13
// CHECK: 6
