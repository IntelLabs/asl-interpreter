// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : bits(4), y : bits(4)) => bits(4)
begin
    return x AND y;
end

func main() => integer
begin
    print(Test('1100', '1010')); println();
    return 0;
end

// CHECK: 4'x8
