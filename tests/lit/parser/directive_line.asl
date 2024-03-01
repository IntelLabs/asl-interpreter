// RUN: not %asli --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

# 42 "foobar.asl"
x == 0
// CHECK: Parser error
// CHECK: file "foobar.asl" line 42 char 0 - 1
