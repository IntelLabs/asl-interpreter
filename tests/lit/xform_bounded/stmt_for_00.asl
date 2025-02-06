// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck --check-prefix=RUN %s
// RUN: %aslrun -O0 -Obounded --backend=sc %s | filecheck --check-prefix=SC_RUN %s
// Copyright (C) 2024-2024 Intel Corporation

func FUT(x : integer {0..10}) => integer {0..100}
begin
    var s : integer {0..100} = 0;
    for i = 0 to x-1 do
        s = (s + i) as {0..100};
    end
    return s;
end

// XFORM:       func FUT.0{}(x : __sint(5)) => __sint(8)
// XFORM-NEXT:  begin
// XFORM-NEXT:      var s : __sint(8) = asl_resize_sintN.0{1, 8}(i1'x0, 8);
// XFORM-NEXT:      for i : __sint(5) = asl_resize_sintN.0{1, 5}(i1'x0, 5) to asl_sub_sintN.0{5}(x, asl_resize_sintN.0{2, 5}(i2'x1, 5)) do
// XFORM-NEXT:          s = asl_add_int.0{}(s, i) as {0..100};
// XFORM-NEXT:      end
// XFORM-NEXT:      return s;
// XFORM-NEXT:  end

func main() => integer
begin
    print_int_dec(FUT(5)); println();
    // RUN: 10
    // SC_RUN: 0x10
    return 0;
end
