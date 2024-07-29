// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : integer) => boolean
begin
    return IsPowerOfTwo(x);
end

func main() => integer
begin
    print(Test(0)); println();
    print(Test(1)); println();
    print(Test(2)); println();
    print(Test(3)); println();
    print(Test(4)); println();
    return 0;
end

// CHECK: FALSE
// CHECK: TRUE
// CHECK: TRUE
// CHECK: FALSE
// CHECK: TRUE
