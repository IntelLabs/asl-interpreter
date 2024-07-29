// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : integer) => integer
begin
    return pow2_int(x);
end

func main() => integer
begin
    print_int_dec(Test(3)); println();
    print_int_dec(Test(7)); println();
    return 0;
end

// CHECK: 8
// CHECK: 128
