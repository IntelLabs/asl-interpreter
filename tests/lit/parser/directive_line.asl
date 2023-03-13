// RUN: not %asli --nobanner %s | %decolor | filecheck %s

# 42 "foobar.asl"
x == 0
// CHECK: Parser error
// CHECK: file "foobar.asl" line 42 char 0 - 1
