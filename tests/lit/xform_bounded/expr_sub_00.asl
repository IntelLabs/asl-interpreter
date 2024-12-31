// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck %s

// Copyright (C) 2024-2024 Intel Corporation

func FUT(x : integer {0..14}) => integer {-1..13}
begin
    return x - 1;
end

// XFORM-LABEL: func FUT.0{}(x : __sint(5)) => __sint(5)
// XFORM-NEXT:  begin
// XFORM-NEXT:      return asl_sub_sintN.0{5}(x, asl_resize_sintN.0{2, 5}(i2'x1, 5));
// XFORM-NEXT:  end

func main() => integer
begin
    print_int_dec(FUT(14));
    // CHECK: 13
    return 0;
end

