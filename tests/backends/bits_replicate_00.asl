// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : bits(4), y : integer) => bits(4*y)
begin
    return Replicate(x, y);
end

func main() => integer
begin
    print(Test('0001', 3)); println();
    print(Test('0101', 2)); println();
    return 0;
end

// CHECK: 12'x111
// CHECK: 8'x55
