
---
layout:     post
title:      "厄拉多塞筛法"
subtitle:   "素数问题"
date:       2019-08-28 11:30:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Golang
    - 数学问题
    - leetcode
    - 算法
---

#### 厄拉多塞筛法(筛选素数)

> 西元前250年，希腊数学家厄拉多塞(Eeatosthese)想到了一个非常美妙的质数筛法，减少了逐一检查每个数的的步骤，可以比较简单的从一大堆数字之中，筛选出质数来，这方法被称作厄拉多塞筛法(Sieve of Eeatosthese)。具体操作：先将 2~n 的各个数放入表中，然后在2的上面画一个圆圈，然后划去2的其他倍数；第一个既未画圈又没有被划去的数是3，将它画圈，再划去3的其他倍数；现在既未画圈又没有被划去的第一个数 是5，将它画圈，并划去5的其他倍数……依次类推，一直到所有小于或等于 n 的各数都画了圈或划去为止。这时，表中画了圈的以及未划去的那些数正好就是小于 n 的素数。 


![厄拉多塞筛法](/img/in-post/Sieve_of_Eratosthenes_animation.gif)

#### [计算素数](https://leetcode-cn.com/problems/count-primes/)

统计所有小于非负整数 n 的质数的数量。

示例:

输入: 10

输出: 4

解释: 小于 10 的质数一共有 4 个, 它们是 2, 3, 5, 7 。

```golang
func countPrimes(n int) int {
	signs := make([]bool, n + 1)
	for i := 2; i <= n; i++ {
		if !signs[i] && !isPrime(i) {
			signs[i] = true
		}
		for j := i + i; j <= n; j += i {
			signs[j] = true
		}
	}
	count := 0
	for i := 2; i < n; i++ {
		if !signs[i] {
			count += 1
		}
	}
	return count
}

func isPrime(num int) bool {
	numSqrt := int(math.Sqrt(float64(num)))
	for i := 2; i <= numSqrt; i++ {
		if num % i == 0 {
			return false
		}
	}
	return true
}
```

