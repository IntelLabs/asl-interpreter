// RUN: not %asli --nobanner %s | %decolor | FileCheck %s

record R {
    x : integer;
    y : integer;
};

func F() => R
begin
    return R{x = 1};
// CHECK: Type error: record initializer is missing fields y and/or has extra fields
end
