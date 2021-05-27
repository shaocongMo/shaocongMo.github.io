---
layout:     post
title:      "[Erlang] 效率指南阅读笔记"
subtitle:   "Erlang"
date:       2021-05-27 17:00:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Erlang
---

##### length
在某些情况下可用模式匹配替代
```erlang
test(L) when length(L) >= 3 ->
    do_something;
test(_) ->
    skip.
```
```erlang
test([_,_,_|Tail] = L) ->
    do_something;
test(_) ->
    skip.
```
一个细微的区别是length(L)如果L是一个不正确的列表，而第二个代码片段中的模式接受一个不正确的列表

##### size
* 返回元祖和二进制数据的大小。使用BIF函数tuple_size/1、byte_size/1会更高效
* 由于使用BIF编译器和运行时系统更多的优化机会。另一个优势是BIF为透析器提供了更多的类型信息。

#### 运算符"--"
* "--"运算符的复杂度与其操作数长度的乘积成正比。这意味着如果运算符的两个操作数都是长列表，则运算符非常慢
* 使用STDLIB中的模块ordsets
```erlang
HugeSet1 = ordsets:from_list(HugeList1),
HugeSet2 = ordsets:from_list(HugeList2),
ordsets:subtract(HugeSet1, HugeSet2)

%% 或者

Set = gb_sets:from_list(HugeList2),
[E || E <- HugeList1, not gb_sets:is_element(E, Set)]
```

#### erlc 编译
* -S 生成汇编文件
* +bin_opt_info 输出二进制优化

#### 列表推导
列表推导编译后会优化成尾递归，编译时加-S参数可以看到汇编文件，查看具体的执行流程
```erlang
[Expr(E) || E <- List]
%% 编译后转化成本地函数
'lc^0'([E|Tail], Expr) ->
    [Expr(E)|'lc^0'(Tail, Expr)];
'lc^0'([], _Expr) -> [].
```

#### lists:flatten/1
避免使用lists:flatten/1,该方法会构造一个全新的列表，甚至比"++"运算符还要糟糕
* 将数据发送到端口时。端口理解深度列表，因此在将列表发送到端口之前，没有理由将其扁平化。
* 调用接受深列表的BIF时，如list_to_binary/1或iolist_to_binary/1...
* 当您知道您的列表只有一级深度时，可以使用lists:append/1...

#### 性能排查demo

```erlang
cprof:start(),R=calendar:day_of_the_week(1896,4,27),cprof:pause(),R.

cprof:analyse(calendar).
% {calendar,9,
%           [{{calendar,df,2},1},
%            {{calendar,dm,1},1},
%            {{calendar,dy,1},1},
%            {{calendar,last_day_of_the_month1,2},1},
%            {{calendar,last_day_of_the_month,2},1},
%            {{calendar,is_leap_year1,1},1},
%            {{calendar,is_leap_year,1},1},
%            {{calendar,day_of_the_week,3},1},
%            {{calendar,date_to_gregorian_days,3},1}]}
cprof:stop().

fprof:start(),
fprof:apply(M, F, A),
fprof:profile(),
fprof:analyse([{dest, "fprof.analysis"},{sort,own}]),
fprof:stop().

```


#### 记一次cup占用过高问题排查 （50% 降低至 20%）

###### 查找cup占用较高的进程 etop 模块
```erlang
% cup占用
spawn(fun() -> etop:start([{interval, 10}, {sort, runtime}]) end).

% 消息队列
spawn(fun() -> etop:start([{interval, 10}, {sort, msg_q}]) end).

% 内存
spawn(fun() -> etop:start([{interval, 10}, {sort, memory}]) end).

% 停止
etop:stop()
```

```shell
========================================================================================
 'node name'                                               02:22:21
 Load:  cpu         1               Memory:  total     1274317    binary      14109
        procs    1596                        processes 1009762    code        15398
        runq        0                        atom          791    ets        190380
                                          
Pid            Name or Initial Func    Time    Reds  Memory    MsgQ Current Function
----------------------------------------------------------------------------------------
<6469.1478.0>  mod:init/1             '-'  272281 2928152       0 gen_server:loop/7   
<6469.624.0>   mod:init/1             '-'  196335  429128       0 gen_server:loop/7   
<6469.775.0>   mod:init/1             '-'  196320 1120272       0 gen_server:loop/7   
<6469.773.0>   mod:init/1             '-'  195253  427848       0 gen_server:loop/7   
<6469.626.0>   mod:init/1             '-'  194656 1117712       0 gen_server:loop/7   
<6469.31963.12>mod:init/1             '-'  190817  690864       0 gen_server:loop/7   
<6469.6869.15> mod:init/1             '-'  190817  690864       0 gen_server:loop/7   
<6469.7847.2>  mod:init/1             '-'  190814  690864       0 gen_server:loop/7   
<6469.1168.8>  mod:init/1             '-'  190278  427848       0 gen_server:loop/7   
<6469.1217.0>  mod:init/1             '-'  190278  427848       0 gen_server:loop/7   
========================================================================================
```

###### 对高占用进程进行性能分析 eprof 模块
```erlang

%% 开始对特定进程分析
Pids = [pid(0,1478,0)].
eprof:start().
eprof:profile(Pids).


%% 停止
eprof:stop_profiling().

%% 在执行eprof:analyze()前执行设置日志文件，可将分析结果保存到文件中
eprof:log("pid1478.analyze").

%% 分析结果
eprof:analyze().
```

* 对分析结果查看，得知部分ets:select/2消耗时间较长，于是对查询结果加入缓存（k/v cache），下次查询直接冲缓存中获取
* k/v cache 可独立进程，将系统中的缓存集中存放，避免各自进程独自处理存放，加大内存开销

###### 加入缓存处理后，cup占用依旧高居不下

于是对进程进行分析发现，存在许多废弃没有关闭的进程。对进程代码修正，加入自动关闭进程功能，cup占用就降低了。

在监督树下的用 supervisor:terminate_child(Sup, ID) 关闭
关闭后需清理监督树的子进程监督信息 supervisor:delete_child(Sup, ID) (看源码 terminate_child 后是有清除数据的，但是没起作用，待查)

非监督树下的进程返回 {stop, normal, State}

##### 总结
* 对耗时的数据查询，加入缓存处理
* 使用全局缓存，避免同样的数据重复存储，占用内存
* 进程都需有关闭处理，废弃闲置进程需关闭
