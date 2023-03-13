// RUN: not %asli --nobanner %s | %decolor | filecheck %s

record R{
    x : integer;
    y : integer;
};

func T() => R
begin
    return R{x=1, z=3};
// CHECK: Type error: record initializer is missing fields y and/or has extra fields z
end
