// RUN: not %asli --nobanner %s | %decolor | FileCheck %s

type B of bits(8) {
    [7]    X
    [6]    X
// CHECK: Type error: fieldname `X` is declared multiple times
};
