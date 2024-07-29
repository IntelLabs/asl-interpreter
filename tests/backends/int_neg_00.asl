// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : integer) => integer
begin
    return -x;
end

func main() => integer
begin
    print_int_dec(Test(4)); println();
    print_int_dec(Test(-5)); println();
    print_int_dec(Test(0)); println();
    return 0;
end

// CHECK: -4
// CHECK: 5
// CHECK: 0
