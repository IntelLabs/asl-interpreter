// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func FUT(x : __sint(8), y : __sint(8)) => boolean
begin
    return asl_eq_sintN(x, y);
end

func main() => integer
begin
    print(FUT(i8'd1, i8'd2)); println();
    // CHECK: FALSE
    print(FUT(i8'd1, i8'd1)); println();
    // CHECK: TRUE
    return 0;
end
