// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func main() => integer
begin
    print_int_hex(42); println();
    print_int_hex(-42); println();
    return 0;
end

// CHECK: 0x2a
// CHECK: -0x2a
