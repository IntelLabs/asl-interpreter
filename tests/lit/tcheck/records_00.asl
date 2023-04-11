// RUN: not %asli --nobanner %s | %decolor | filecheck %s

record R {
    x : integer;
    x : integer;
// CHECK: Type error: fieldname `x` is declared multiple times
};
