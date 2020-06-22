common-lisp-user 的别名是 cl-user
in-package 是指定待扣押的包
defpackage 是定义包
一般是先定义一个 package 然后再使用 in-package 指定扣押包
progn 多个表达式执行, 返回最后一个表达式的值
defsystem 定义一个 system
defmethod 定义一个 method
defparameter 定义一个 parameter
从基类实现的类, 会先执行具体类在执行基类的方法, 即就近原则
