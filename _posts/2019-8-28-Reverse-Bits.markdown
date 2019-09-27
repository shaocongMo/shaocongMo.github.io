---
layout:     post
title:      "二进制反转"
subtitle:   "位运算"
date:       2019-08-28 20:00:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Golang
    - 位运算
---

颠倒给定的 32 位无符号整数的二进制位。

```golang
func reverseBits(num uint32) uint32 {
    var i uint32
    for i = 0; i <= 15; i++ {
        head, tail := BitGet(num, i), BitGet(num, 31 - i)
        num = BitSet(num, i, tail)
        num = BitSet(num, 31 - i, head)
    }
    return num
}

func BitGet(num uint32, pos uint32) uint32 {
    // 获得第pos位的值
    return (num & (1 << pos)) >> pos
}

func BitSet(num uint32, pos uint32, value uint32) uint32 {
    // 重置第pos位为0
    num = num & ( ^(1 << pos))
    // 设置第pos位为：value
    return num | (value << pos)
}
```