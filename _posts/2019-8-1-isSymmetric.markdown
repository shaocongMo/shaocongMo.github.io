---
layout:     post
title:      "对称二叉树"
subtitle:   "Leetcode"
date:       2019-08-1 14:50:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Golang
    - 二叉树
    - 算法
    - 递归
---

#### 思路

迭代：
对于树的每一层，都把对应匹配节点放到队列中，每次匹配检测都从队列头取两个节点（对应匹配的节点），进行检测

图解：
![img](/img/in-post/isSymmetricTree.png)
如上图树
生成队列依次为：[B, C] -> [D, G, E, F]

递归思路基本与迭代一致

```golang
package main

import (
    "fmt"
)

func main() {
    fmt.Println(isSymmetric(&TreeNode{Val:1, Left:&TreeNode{Val:2,}, Right:&TreeNode{Val:3,}}))
    fmt.Println(isSymmetric(&TreeNode{Val:1, Left:&TreeNode{Val:3},}))
    fmt.Println(isSymmetric(&TreeNode{Val:1, Left:&TreeNode{Val:2,Left:&TreeNode{Val:3,},Right:&TreeNode{Val:4,}}, Right:&TreeNode{Val:2,Left:&TreeNode{Val:4},Right:&TreeNode{Val:3,}}}))
    fmt.Println(isSymmetric(&TreeNode{Val:1, Left:&TreeNode{Val:2,Left:&TreeNode{Val:3,},Right:nil}, Right:&TreeNode{Val:2,Left:nil,Right:&TreeNode{Val:3,}}}))

    tree := &TreeNode{Val:1, 
                      Left:&TreeNode{Val:2, Left:&TreeNode{Val:3,Left:&TreeNode{Val:1,}, Right:&TreeNode{Val:2,}}}, 
                      Right:&TreeNode{Val:2,Right:&TreeNode{Val:3,Left:&TreeNode{Val:2,}, Right:&TreeNode{Val:1,}}}}
    fmt.Println(isSymmetric(tree))
}

type TreeNode struct {
    Val int
    Left *TreeNode
    Right *TreeNode
}

func isSymmetric(root *TreeNode) bool {
    // 迭代解法
    if root != nil {
        queue := []*TreeNode{}
        queue = append(queue, root.Left)
        queue = append(queue, root.Right)
        for len(queue) > 0 {
            left := queue[0]
            right := queue[1]
            queue = queue[2:]
            if left == nil && right == nil {
                continue
            }
            if left != nil && right != nil && left.Val == right.Val {
                queue = append(queue, left.Left)
                queue = append(queue, right.Right)
                queue = append(queue, left.Right)
                queue = append(queue, right.Left)
            } else {
                return false
            }
        }
    }
    return true

    递归解法
    if root != nil {
        return isSymmetricCheck(root.Left, root.Right)
    }
    return true
}

func isSymmetricCheck(left, right *TreeNode) bool {
    if left == nil && right == nil {
        return true
    }
    if left != nil && right != nil && left.Val == right.Val {
        return isSymmetricCheck(left.Left, right.Right) && isSymmetricCheck(left.Right, right.Left)
    }
    return false
}
```

#### [原题内容](https://leetcode-cn.com/explore/interview/card/top-interview-questions-easy/7/trees/49/)

给定一个二叉树，检查它是否是镜像对称的。

```
例如，二叉树 [1,2,2,3,4,4,3] 是对称的。

    1
   / \
  2   2
 / \ / \
3  4 4  3
但是下面这个 [1,2,2,null,3,null,3] 则不是镜像对称的:

    1
   / \
  2   2
   \   \
   3    3
说明:
如果你可以运用递归和迭代两种方法解决这个问题，会很加分。
```