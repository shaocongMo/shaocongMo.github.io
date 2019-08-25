---
layout:     post
title:      "N人过桥问题"
subtitle:   "Leetcode"
date:       2019-08-20 15:30:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Golang
    - 动态规划
    - 算法
---

#### 题目

> 在一个夜黑风高的晚上，有n（n <= 50）个小朋友在桥的这边，现在他们需要过桥，但是由于桥很窄，每次只允许不大于两人通过，他们只有一个手电筒，所以每次过桥的两个人需要把手电筒带回来，i号小朋友过桥的时间为T[i]，两个人过桥的总时间为二者中时间长者。问所有小朋友过桥的总时间最短是多少。

#### 分析

**n == 1**： 一个人过桥，过桥时间为：T[1]

**n == 2**： 两个人过桥，过桥时间位：T[2]

**n == 3**： 三个人过桥 [a, b, c] 过桥时间递增排序，过桥顺序： a和c过桥 -> a回来 -> a和b过桥 过桥时间为：T[3] + T[1] + T[2]

**n >= 4**： 四人及以上过桥 [a, b, c .... x, y, z]  y、z为过桥最慢的人 每次用最快两个人把手电筒送回来，把最慢两个人送过去，保持手电筒在过桥这一边 人数变化： n - 2 变成 求：n-2个人的过桥问题

过桥方案： 
* 1、 ab -> a -> yz -> b ： T[2] + T[1] + T[n] + T[2] -> T[1] + 2T[2] + T[n]
* 2、 az -> a -> ay -> a ： T[n] + T[1] + T[n - 1] + T[1] -> 2T[1] + T[n-1] + T[n]
* 最优方案：min(T[1] + 2T[2] + T[n], 2T[1] + T[n-1] + T[n])

#### 实现代码
```golang
func crossBridge(spends []int) int {
    n := len(spends)
    totalSpend := 0
    for n > 0 {
        if n == 1 {
            totalSpend += spends[n - 1]
            n = n - 1
        } else if n == 2 {
            totalSpend += spends[n - 1]
            n = n - 2
        } else if n == 3 {
            totalSpend += spends[n - 3] + spends[n - 2] + spends[n - 1]
            n = n - 3
        } else {
            spend1 := spends[0] + spends[1] * 2 + spends[n - 1]
            spend2 := spends[0] * 2 + spends[n - 2] + spends[n - 1]
            if spend1 > spend2 {
                totalSpend += spend2
            } else {
                totalSpend += spend1
            }
            n = n - 2
        }
    }
    return totalSpend
}
```