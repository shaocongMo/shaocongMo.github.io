---
layout:     post
title:      "最长回文子串"
subtitle:   "位运算"
date:       2019-09-27 17:30:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Golang
    - 算法
    - leetcode
---

#### [原题内容](https://leetcode-cn.com/problems/longest-palindromic-substring/)

给定一个字符串 s，找到 s 中最长的回文子串。你可以假设 s 的最大长度为 1000。

示例 1：

> 输入: "babad"
> 输出: "bab"
> 注意: "aba" 也是一个有效答案。

示例 2：

> 输入: "cbbd"
> 输出: "bb"

#### 中心拓展法

思路：把每个索引当做回文串的中心，进行拓展，寻找以该索引为中心的最长回文串。注意回文串的奇偶问题

![回文串的奇偶问题](/img/in-post/2019-9-27-palindrome.png)

```golang
func longestPalindrome(s string) string {
    maxPalindrome, maxLen := "", 0
    for i := 0; i < len(s); i++ {
        palindrome_odd, odd_len := longestPalindromeCenter(s, i, i)
        palindrome_even, even_len := longestPalindromeCenter(s, i, i + 1)
        if odd_len > even_len && odd_len > maxLen {
            maxPalindrome = palindrome_odd
            maxLen = odd_len
        } else if even_len > odd_len && even_len > maxLen {
            maxPalindrome = palindrome_even
            maxLen = even_len
        }
    }
    return maxPalindrome
}

func longestPalindromeCenter(s string, left, right int) (string, int) {
    for left >= 0 && right < len(s) && s[left] == s[right] {
        left -= 1
        right += 1
    }
    return s[left + 1: right], right - left
}
```

复杂度分析：

* 时间复杂度：O(N^2)
* 空间复杂度：O(1)