---
layout:     post
title:      "198.打家劫舍"
subtitle:   "Leetcode"
date:       2019-08-20 19:30:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Golang
    - 动态规划
    - 算法
---

#### 思路
样例分析，如：[1, 2, 3, 1]
DFS分析：
![img](/img/in-post/houserobber.png)
如上图分析可得推导公式：设 money(i) 为偷窃到第 i 号房的最优解, T(i) 为第 i 号房所拥有的现金数
* i > n : money(i) = 0
* i <= n : money(i) = max(money(i - 1), money(i - 2) + T(i))
在DFS分析过程中，还可以看到在求解过程中，**存在子问题**，于是考虑动态规划。 DFS会超时
设dp[i] 为偷窃到第 i 号房的最优解 dp[n] 为最终答案
dp[0] = 0
dp[1] = T(1)
dp[i] = max(dp[i - 1], dp[i - 2] + T(i))

#### 代码
```golang
func dpRob(nums []int) int {
    if len(nums) == 0 {
        return 0
    }
    dp := make([]int, len(nums) + 1)
    dp[0] = 0
    dp[1] = nums[0]
    for i := 2; i <= len(nums); i ++ {
        if dp[i - 1] > nums[i - 1] + dp[i - 2] {
            dp[i] = dp[i - 1]
        } else {
            dp[i] = nums[i - 1] + dp[i - 2]
        }
    }
    return dp[len(nums)]
}
```

#### [原题内容](https://leetcode-cn.com/problems/house-robber)
你是一个专业的小偷，计划偷窃沿街的房屋。每间房内都藏有一定的现金，影响你偷窃的唯一制约因素就是相邻的房屋装有相互连通的防盗系统，如果两间相邻的房屋在同一晚上被小偷闯入，系统会自动报警。

给定一个代表每个房屋存放金额的非负整数数组，计算你在不触动警报装置的情况下，能够偷窃到的最高金额。

示例 1:

输入: [1,2,3,1]

输出: 4

解释: 偷窃 1 号房屋 (金额 = 1) ，然后偷窃 3 号房屋 (金额 = 3)。
     偷窃到的最高金额 = 1 + 3 = 4 。

示例 2:

输入: [2,7,9,3,1]

输出: 12

解释: 偷窃 1 号房屋 (金额 = 2), 偷窃 3 号房屋 (金额 = 9)，接着偷窃 5 号房屋 (金额 = 1)。
     偷窃到的最高金额 = 2 + 9 + 1 = 12 。
