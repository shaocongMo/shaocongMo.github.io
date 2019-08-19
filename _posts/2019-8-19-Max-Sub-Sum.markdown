---
layout:     post
title:      "最大子序列和"
subtitle:   "Leetcode"
date:       2019-08-19 20:30:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Golang
    - 动态规划
    - 算法
---

#### 分治思路
求出 母序列， 左子序列，右子序列的最大值
![img](/img/in-post/sub-max-sum.png)

```golang
func subSum(nums []int, left, right int) int {
    if left == right {
        return nums[left]
    }

    mid := (left + right) / 2
    lsubSum := subSum(nums, left, mid)
    rsubSum := subSum(nums, mid + 1, right)

    lsum, rsum := nums[mid], nums[mid + 1]
    for i, sum := mid, 0; i >= left; i-- {
        sum += nums[i]
        if sum > lsum {
            lsum = sum
        }
    }
    for i, sum := mid + 1, 0; i <= right; i++ {
        sum += nums[i]
        if sum > rsum {
            rsum = sum
        }
    }

    subSum := lsum + rsum
    if lsubSum > subSum {
        subSum = lsubSum
    }
    if rsubSum > subSum {
        subSum = rsubSum
    }

    return subSum
}
```

#### 动态规划思路(摘自LeetCode题解)

这道题用动态规划的思路并不难解决，比较难的是后文提出的用分治法求解，但由于其不是最优解法，所以先不列出来
动态规划的是首先对数组进行遍历，当前最大连续子序列和为 sum，结果为 ans
如果 sum > 0，则说明 sum 对结果有增益效果，则 sum 保留并加上当前遍历数字
如果 sum <= 0，则说明 sum 对结果无增益效果，需要舍弃，则 sum 直接更新为当前遍历数字
每次比较 sum 和 ans的大小，将最大值置为ans，遍历结束返回结果
时间复杂度：O(n)O(n)

作者：guanpengchn
链接：https://leetcode-cn.com/problems/maximum-subarray/solution/hua-jie-suan-fa-53-zui-da-zi-xu-he-by-guanpengchn/

```goland
func dpSubSum(nums []int) int {
    sum := 0
    ans := nums[0]
    for i := 0; i < len(nums); i++ {
        if sum > 0 {
            sum = sum + nums[i]
        } else {
            sum = nums[i]
        }
        if sum > ans {
            ans = sum
        }
    }
    return ans
}
```

#### [原题内容](https://leetcode-cn.com/problems/maximum-subarray/)

给定一个整数数组 nums ，找到一个具有最大和的连续子数组（子数组最少包含一个元素），返回其最大和。

示例:

输入: [-2,1,-3,4,-1,2,1,-5,4],
输出: 6
解释: 连续子数组 [4,-1,2,1] 的和最大，为 6。
进阶:

如果你已经实现复杂度为 O(n) 的解法，尝试使用更为精妙的分治法求解。

