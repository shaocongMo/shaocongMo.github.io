---
layout:     post
title:      "验证二叉搜索树"
subtitle:   "Leetcode"
date:       2019-07-30 16:30:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Golang
    - 二叉树
    - 算法
---

#### 解法一

对整个树进行中序遍历，得到的遍历结果是一个自增列表
优化：在遍历过程中，若碰到前一个节点值大于后一个节点值，则返回False

```golang
func isValidBST(root *TreeNode) bool {
    if root == nil {
        return true
    }
    result, _ := inorderTraversal(root, nil)
    return result
}

// 中序遍历
func inorderTraversal(root *TreeNode, lastNode *TreeNode) (bool, *TreeNode) {
    leftCheck := true
    if root.Left != nil {
        leftCheck, lastNode = inorderTraversal(root.Left, lastNode)
    }
    if leftCheck == false {
        return false, nil
    }

    if lastNode == nil || lastNode.Val < root.Val {
        lastNode = root
    } else {
        return false, nil
    }

    rightCheck := true
    if root.Right != nil {
        rightCheck, lastNode = inorderTraversal(root.Right, lastNode)
    }
    if rightCheck == false {
        return false, nil
    }
    return true, lastNode
}
```

#### 解法二

在任何一颗树中，所有节点值都在一定范围内（根节点范围：MinInt64 ~ MaxInt64）

如： 输入:[5, 1, 7, null, null, 6, 8, null, null, null, null]

![img](/img/in-post/vaild_BST.png)

每个子树节点范围：
5: MinInt64 ~ MaxInt64
1: MinInt64 ~ 5
7: 5 ~ MaxInt64
3: 5 ~ 6
8: 7 ~ MaxInt64

对树进行BFS遍历，判断每个节点值是否在合理范围内，若不在则停止遍历

```golang
func isValidBST(root *TreeNode) bool {
    return isValidBstUtils(root, math.MinInt64 , math.MaxInt64)
} 

func isValidBstUtils(root *TreeNode, min, max int)bool{
    if root == nil{
        return true
    }
    if root.Val <= min || root.Val >= max{
        return false
    }

    return isValidBstUtils(root.Left, min, root.Val) && isValidBstUtils(root.Right, root.Val, max)
}
```

#### [原题内容](https://leetcode-cn.com/explore/interview/card/top-interview-questions-easy/7/trees/48/)

给定一个二叉树，判断其是否是一个有效的二叉搜索树。

假设一个二叉搜索树具有如下特征：

* 节点的左子树只包含小于当前节点的数。
* 节点的右子树只包含大于当前节点的数。
* 所有左子树和右子树自身必须也是二叉搜索树。

#### 示例1
输入: [2, 1, 3]
输出: true

#### 示例2
输入: [5,1,4,null,null,3,6]
输出: false
解释: 根节点的值为 5 ，但是其右子节点值为 4