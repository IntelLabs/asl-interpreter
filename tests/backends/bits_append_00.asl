// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : bits(4), y : bits(4)) => bits(8)
begin
    return [x, y];
end

func main() => integer
begin
    print(Test('1101', '0010')); println();
    return 0;
end

// CHECK: 8'xd2
