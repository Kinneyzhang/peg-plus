使用 define-peg-rule+ 定义规则时
宏参数：指使用 define-peg-rule+ 宏定义规则时的入参
规则参数：指规则的具体定义中使用到的规则的参数

如下面这个例子：

```
(define-peg-rule+ group-to-pex (pex)
  ;; 从当前位置开始捕获，直到匹配 pex 前为止
  (group (any-to-pex pex)))
```

- (pex) 是宏参数列表
- (any-to-pex pex) 是 group 的规则参数
- pex 是 any-to-pex 的规则的参数

在规则的定义中，何处的参数使用 funcall 包裹？
1. 参数属于宏参数列表
2. 该参数的规则为内置规则 且 不是 guard
说明：
位于 guard 规则中的内容是 elisp 表达式，无需特殊处理
位于 `(.. -- ..) 表达式中的内容 elisp 表达式，无需特殊处理
如果是自定义规则，则属于规则参数透传的情况，无需 funcall
只有在最底层的内置规则使用参数时，才需要 funcall

在规则的定义中，何处的参数使用 peg 包裹？
1. 内置规则的参数无需 peg 包裹，自定义规则的部分参数需要
2. 自定义规则的参数分为两类：一类是 pex(peg表达式)，一类是 exp(elisp表达式)
3. 是pex格式的参数 且 不属于宏参数时，使用 peg 包裹
4. exp格式的参数无需 peg 包裹
5. 忽略 `(.. -- ..) 中的内容

