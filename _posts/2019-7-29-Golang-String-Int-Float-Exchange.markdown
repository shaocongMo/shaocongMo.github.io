---
layout:     post
title:      "Golang 中 int, uint, float 与 string 的相互转换"
subtitle:   "Golang"
date:       2019-07-29 16:00:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Golang
    - 类型转换
    - 字符串
---

主要涉及到包[strconv](https://golang.org/pkg/strconv), 具体详情自行查看

相关转换示例如下

```golang
package main

import (
	"strconv"
	"fmt"
)

func main() {
	var intnum int = -666
	var int64num int64 = -64
	var uintnum uint64 = 666
	floatnum := 6.66

	str := ""

	str = strconv.Itoa(intnum)
	fmt.Println("int to string: ", intnum, "=>", str)

	// strconv.FormatInt
	// 第二个参数base: 表示转换的进制 2 <= base <= 64
	str = strconv.FormatInt(int64num, 10)
	fmt.Println("int64 to string: ", int64num, "=>", str)

	str = strconv.FormatUint(uintnum, 10)
	fmt.Println("uint to string: ", uintnum, "=>", str)

	// strconv.FormatFloat
	// 第二个参数为转换格式参数
	// 'b' (-ddddp±ddd，二进制指数)
	// 'e' (-d.dddde±dd，十进制指数)
	// 'E' (-d.ddddE±dd，十进制指数)
	// 'f' (-ddd.dddd，没有指数)
	// 'g' ('e':大指数，'f':其它情况)
	// 'G' ('E':大指数，'f':其它情况)
	// 
	// 第三个参数为保留小数个数（-1为保持原来不变, 其他值依据四舍五入进行处理）
	// 第四个参数为Float的位数(bitSize)
	str = strconv.FormatFloat(floatnum, 'E', -1, 32)
	fmt.Println("float to string: ", floatnum, "=>", str)

	str = strconv.FormatFloat(floatnum, 'f', -1, 64)
	fmt.Println("float to string: ", floatnum, "=>", str)

	str = strconv.FormatFloat(floatnum, 'f', 1, 64)
	fmt.Println("float to string: ", floatnum, "=>", str)

	intstr := "-6661"
	uintstr := "6661"
	floatstr :=  "6.661"

	var err error

	intnum, err = strconv.Atoi(intstr)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println("string to int:", intstr, "=>", intnum)
	}

	// 第三参数为位数(bitSize)
	uintnum, err = strconv.ParseUint(uintstr, 10, 32)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println("string to unit:", uintstr, "=>", uintnum)
	}

	floatnum, err = strconv.ParseFloat(floatstr, 64)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println("string to flost:", floatstr, "=>", floatnum)
	}
}
```