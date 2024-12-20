---
layout: post
title: Using the ASL interpreter
---

The ASL interpreter reads ASL files specified on the command line and
provides an interactive environment for executing ASL
statements and expressions.

```
    $ asli
                _____  _       _
        /\     / ____|| |     (_)   ASL interpreter
       /  \   | (___  | |      _    Copyright Arm Limited (c) 2017-2019
      / /\ \   \___ \ | |     | |   Copyright (C) 2022-2024 Intel Corporation
     / ____ \  ____) || |____ | |
    /_/    \_\|_____/ |______||_|   ASLi 1.0.0

    Type :? for help
    ASLi> 1+1
    2
    ASLi> ZeroExtend('11', 32)
    32'x3
    ASLi> let x : bits(32) = ZeroExtend('11', 32);
    ASLi> x
    32'x3
    ASLi> :quit
```

The ASL interpreter needs `prelude.asl` which is part of this repository. You
either run the ASL interpreter from a directory containing `prelude.asl` or run
the ASL interpreter from anywhere by setting `ASL_PATH` to point to a
directory containing `prelude.asl`.

