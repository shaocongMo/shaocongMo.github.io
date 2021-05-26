---
layout:     post
title:      "[Erlang] Comment Test浅尝"
subtitle:   "leetcode"
date:       2021-05-26 17:00:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Erlang
    - 测试
---

> 通过Common Test测试代码，用Cover Test测试代码的覆盖率

构建简单的测试例子，用模式匹配来检测运行是否出错

目录结构
```
|-
  |-check
    |-check_demo.bat
    |-cover.spec
  |-src
    |-demo.erl
    |-demo_SUITE.erl
  |-Emakefile
```

```erlang
-module(demo_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).

all() -> 
	[test1, 
	test2,
	test3,
	test4].

test1(_Config) ->
	1 = 1.

test2(_Config) ->
	A = 0,
	1/A.

test3(_Config) ->
	2 = demo:check(2).

test4(_Config) ->
	1 = demo:check(5),
	4 = demo:check(3).

```

```erlang
-module(demo).

-export ([check/1]).

check(3) -> 4;
check(2) -> 2;
check(_) -> 1.
```

代码覆盖测试配置
cover.spec 里面的路径相对于配置文件路径
进行代码覆盖测试前提，编译的代码需要加上debug_info参数
```
{level, details}.
{incl_dirs, ["../ebin"]}.
```

启动测试，测试完成后会生成html文件到demo目录
-dir 路径相对于跟目录
-cover 路径相对于根目录
-logdir 路径相对于跟目录
-pa 路径相对于输出目录（logdir）
```bash
mkdir demo
cd ../
erl -make
ct_run -dir ./src -no_auto_compile -suite demo_SUITE -cover ./check/cover.spec -logdir ./check/demo -pa ../../../ebin 
```
