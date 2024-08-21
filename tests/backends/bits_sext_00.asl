// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test_3_8(x : bits(3)) => bits(8)
begin
    return asl_sign_extend_bits(x, 8);
end

func Test_70_140(x : bits(70)) => bits(140)
begin
    return asl_sign_extend_bits(x, 140);
end

func main() => integer
begin
    print_bits_hex(Test_3_8('110')); println();
    // CHECK: 8'xfe
    print_bits_hex(Test_3_8('011')); println();
    // CHECK: 8'x3

    print_bits_hex(Test_70_140(16384[0 +: 70])); println();
    // CHECK: 140'x4000
    print_bits_hex(Test_70_140((-16384)[0 +: 70])); println();
    // CHECK: 140'xfffffffffffffffffffffffffffffffc000
    return 0;
end
