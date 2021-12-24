---
layout:     post
title:      "[Erlang] dbg:tracer用法"
subtitle:   "Erlang"
date:       2021-12-24 15:00:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Erlang
---

#### dbg:tracer用法
##### dbg:tracer().
    开启dbg的tracer
    dbg:p(Item, Flags) -> {ok, MatchDesc} | {error, term()}
    设置跟踪属性.
    第一个参数Item是设定要跟踪的对象：
        如果Item是一个pid()，则只会trace 对应的进程，如果是在集群中，只要节点在traced nodes列表内，可以跨节点trace那个进程.
        如果Item是一个atom all,就会trace 整个系统所有的进程，同样可以监控整个集群。
        如果Item是一个atom new,就会trace 系统在此刻开始所创建的新进程，可以监控整个集群。
        如果Item是一个atom existing，就会trace 系统此刻之前所创建的进程，可以监控整个集群。
    第二个参数Flags是设置要监听的动作:
        s(send) trace 进程发送的消息
        r(receive) trace 进程受到的消息
        m(messages) trace 进程收的或者发的消息 -.- m = s+r
        c(call） 通过tp/2设置的匹配规则后，可以trace 进程所有的call匹配的动作。
        p（procs） trace 与目标进程相关联的进程
        sos(set on spwan) 使监控的进程所产生的进程继承所以trace flags
        sol （set on link） 如果有另外的进程p2 link被监控的进程，p2会继承所有的trace flags
        all 所有的flags
        clear 清除所有flags\

    dbg:tp({Module, Function, Arity}, MatchSpec) -> {ok, MatchDesc} | {error, term()}
    dbg:tpl({Module, Function, Arity}, MatchSpec) -> {ok, MatchDesc} | {error, term()}
    tpl和tp类似，只是tpl会trace 未导出函数。
        dbg:tpl(Module, '_', []). 会trace 该module的所有的调用。
        dbg:tpl(Module, Function, '_', []). 会trace module：function，不限定参数个数。
        dbg:tpl(Module, Function, Arity, []). trace module：function/arity。
        dbg:tpl(M, F, A, [{'_', [], [{return_trace}]}]). 会跟踪返回值。

```erlang
1> dbg:tracer().
{ok,<0.61.0>}
2> 
2> dbg:p(self(), [call]).
{ok,[{matched,nonode@nohost,1}]}
3> 
3> dbg:tp(dbg, '_', []).
{ok,[{matched,nonode@nohost,66}]}
4> 
4> dbg:get_tracer().
(<0.59.0>) call dbg:get_tracer()
{ok,<0.62.0>}
5> 
5> dbg:tp(dbg, '_', [{'_', [], [{return_trace}]}]).
(<0.59.0>) call dbg:tp(dbg,'_',[{'_',[],[{return_trace}]}])
(<0.59.0>) call dbg:module_info()
{ok,[{matched,nonode@nohost,66},{saved,1}]}
6> 
6> dbg:get_tracer().
(<0.59.0>) call dbg:get_tracer()
(<0.59.0>) returned from dbg:get_tracer/0 -> {ok,<0.62.0>}
{ok,<0.62.0>
```