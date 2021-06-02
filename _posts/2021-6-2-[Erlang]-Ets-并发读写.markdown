---
layout:     post
title:      "[Erlang] ets 并发读写"
subtitle:   "Erlang"
date:       2021-06-02 20:00:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Erlang
---

ETS是Erlang内置的内存数据库，可用于多进程共享数据，具有并发读写的性能。

比如新建一个person的ets表：
```erlang
ets:new(person, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]).
```

说说ets并发读写的两个参数：write_concurrency / read_concurrency

##### write_concurrency（并发写）
> 可以提高多进程并发写ets的效率。通常来说，ets写数据时整张表是锁定的，其他进程不能进行读写直到前面的操作完成。并发写可以改变这个情况，同一个表中的不同记录可以被多个进程并发读写。有了这个参数，使得ets写记录时表读写锁变成了读锁，就是说，只要不是同一条记录，还可以继续往这个ets表写入数据，提高了并发写效率。但 并发写也有弊端， 降低 数据 连续写入的效率和性能。如果有且只有一个进程在读写数据，将会带来一定的开销。而测试发现这个开销比较小，可以忽略。而且，只有一个进程在读写数据的场合比较小。

所以，并发写的适用场合如下：
* 数据并发读写很频繁
* 并发读写的数据量比较少（取记录数）

但是，像这样一次性插入多条记录，ets要保证原子性，并发效率会大打折扣：

```erlang
ets:insert(person, [{john, 28}, {lucy, 25}, {tom, 2}]).
```

```c
  /* ets:insert/2 的实现(erl_db.c)
   * 如果第2个参数是列表，就锁表，不是就锁记录
   */
 
  /* Write lock table if more than one object to keep atomicy */
    kind = ((is_list(BIF_ARG_2) && CDR(list_val(BIF_ARG_2)) != NIL)
	    ? LCK_WRITE : LCK_WRITE_REC);
```

另外，这个参数目前不支持ordered_set 类的ets表

##### read_concurrency（并发读）
> 优化ets并发读性能，特别是在多核smp的支持下，读操作变得比较廉价。但是同时也带来一个问题，读写操作之间的切换消耗更多的性能。
所以，并发读的适用场合如下：
* 读比写更加频繁
* 大量的读少量写，大量的写少量读

什么时候使用这两个参数？
* 使用write_concurrency参数大多时候是有效的，测试写操作性能提高3~4倍，而且，在单进程写多进程读的场合下也同样适用。如果并发读写次数较少，而且每次都要读取或者写入大量数据就不适合了。
* read_concurrency的使用要看场合，并不是高并发就适合。如果每次读的数据和写的数据都很少，而且读写都很频繁，就没必要使用这个参数。如果很少写数据，大多时候都是读数据的话用这个参数就很适用了。另外，如果数据读写频繁，但每次读写的数据都很多，也适当考虑用这个参数。
* write_concurrency和read_concurrency可以同时使用，但有一定开销的，适用场合是经常有大量并发读和大量并发写操作。

注意：
* write_concurrency并不会给记录加上记录锁，而是会给一张表分配16个表锁，并发写的进程会来竞争这16个锁。其实也就是实现了一个写进程的进程池而已