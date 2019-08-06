---
layout:     post
title:      "88. 合并两个有序数组"
subtitle:   "Leetcode"
date:       2019-08-06 21:50:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Golang
    - 归并
    - 数组
    - leetcode
---

#### 思路
利用归并方法，从小到大归并，需要另外的空间 O(m)
另外一种归并，从大到小进行归并，空间复杂度：O(1)

两种方法的时间复杂: O(m + n)

```golang
    // 归并
    nums := make([]int, m)
    i, j, k := 0, 0, 0
    for ; k < m; k++ {
        nums[k] = nums1[k]
    }
    k = 0
    for i < m && j < n {
        if nums[i] < nums2[j] {
            nums1[k] = nums[i]
            i += 1
        } else {
            nums1[k] = nums2[j]
            j += 1
        }
        k += 1
    }
    for ; i < m; i++ {
        nums1[k] = nums[i]
        k += 1
    }
    for ; j < n; j++ {
        nums1[k] = nums2[j]
        k += 1
    }

    // 从后面开始合并 双指针
    tail := n + m - 1
    i, j := m-1, n-1
    for i >= 0 && j >= 0 {
        if nums1[i] < nums2[j] {
            nums1[tail] = nums2[j]
            j -= 1
        } else {
            nums1[tail] = nums1[i]
            i -= 1
        }
        tail -= 1
    }
    
    for ; i >= 0; i-- {
        nums1[tail] = nums1[i]
        tail -= 1
    }
    for ; j >= 0; j-- {
        nums1[tail] = nums2[j]
        tail -= 1
    }
```

#### [原题内容](https://leetcode-cn.com/problems/merge-sorted-array/)

给定两个有序整数数组 nums1 和 nums2，将 nums2 合并到 nums1 中，使得 num1 成为一个有序数组。

说明:

初始化 nums1 和 nums2 的元素数量分别为 m 和 n。
你可以假设 nums1 有足够的空间（空间大小大于或等于 m + n）来保存 nums2 中的元素。

示例:

输入:
nums1 = [1,2,3,0,0,0], m = 3
nums2 = [2,5,6],       n = 3

输出: [1,2,2,3,5,6]
