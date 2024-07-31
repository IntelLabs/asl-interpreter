// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : bits(8), y : bits(8)) => bits(8)
begin
    return sub_bits(x, y);
end

func main() => integer
begin
    print(Test(1[0 +: 8], 2[0 +: 8])); println();
    // CHECK: 8'xff
    print(Test(2[0 +: 8], 1[0 +: 8])); println();
    // CHECK: 8'x1
    return 0;
end

