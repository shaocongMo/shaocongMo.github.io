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

#### 动态规划

状态：

dp[l][r]: 第 l 位至第 r 位是否为回文串

状态转移：

字串情况分析：
* 1、当原字符串的元素个数为 3 个的时候，如果左右边界相等，那么去掉它们以后，只剩下 1 个字符，它一定是回文串，故原字符串也一定是回文串；
* 2、当原字符串的元素个数为 2 个的时候，如果左右边界相等，那么去掉它们以后，只剩下 0 个字符，显然原字符串也一定是回文串。
* 结论: 子串去除左右边界后若 r - l <= 2 时 一定位回文串

状态方程:

dp[l][r] = s[l] == s[r] && ( r - l <= 2 || dp[l + 1][r - 1] )


```golang
func longestPalindromeDP(s string) string {
    dp := make([][]bool, len(s))
    for i := 0; i < len(s); i++ {
        dp[i] = make([]bool, len(s))
    }
    max_len, subStr := 0, ""
    for r := 0; r < len(s); r++ {
        for l := 0; l <= r; l++ {
            if s[l] == s[r] && (r - l <= 2 || dp[l + 1][r - 1]) {
                dp[l][r] = true
                cur_len := r - l + 1
                if cur_len > max_len {
                    max_len = cur_len
                    subStr = s[l:r + 1]
                }
            }
        }
    }
    return subStr
}
```