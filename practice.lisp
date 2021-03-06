;; common-lisp-user 的别名是 cl-user
;; in-package 是指定待扣押的包
;; defpackage 是定义包
;; 一般是先定义一个 package 然后再使用 in-package 指定扣押包
;; progn 多个表达式执行, 返回最后一个表达式的值
;; defsystem 定义一个 system
;; defmethod 定义一个 method
;; defparameter 定义一个 parameter
;; 从基类实现的类, 会先执行具体类在执行基类的方法, 即就近原则

#|
#' 仅用来引用那些一符号命名的函数
apply 的最后一个参数必须是list, 而 funcall 则不需要这种
mapcar 将后面的 list 作用到 第一个参数(即需要作用的函数) 后面的每一个 list 都是对应着前面的函数的参数
内联编译: 应该是将调用函数的代码放入被调用函数中才编译? 内联函数有所限制, 基本上和宏差不多

在某些方面宏能够为我们带来更好的性能, 这将会把某些代码的编译时间放到最后去

有时候我们就是需要变量被我们所写的宏所捕获, 就好像是在英语中, 不管这个句子是有多么的长, 那么其中的it始终是指代的一个事物

关于lisp中的读取宏, 目前还有许多的不了解

line 2605 需要仔细阅读

在 common lisp 中, 一个符号的symbol-value和symbol-function是不同的
|#
