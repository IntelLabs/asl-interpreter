// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(w : integer, N : integer) => bits(N)
begin
    return mk_mask(w, N);
end

func main() => integer
begin
    print(Test(3, 8)); println();
    return 0;
end

// CHECK: 8'x7
