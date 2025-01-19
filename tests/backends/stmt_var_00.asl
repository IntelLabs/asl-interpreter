// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer) => integer
begin
    var - = x + 1;
    - = x - 1;
    return x;
end

func main() => integer
begin
    print_int_dec(Test(3)); println();
    // CHECK: 3

    return 0;
end
