( + 1 1 )
(print "Hello World")
(format t "~%~A plus ~A equals ~A. ~%" 2 3 (+ 2 3))
(defun askem (string)
	(format t "~A " string)
	(read))
;; (askem "Tabuyos")

(defun addn (n)
	#'(lambda (x)
		  (+ x n)))

(cons 'a '(b c)) ;; cons 函数是将两个实参进行列表构造，若第二个实参是列表，返回构造的新列表
(cons 'b '(c))
(car '(a b c d)) ;; car 函数是取列表的第一个元素，cdr 函数是取除第一个元素外的元素
(car '((a b) c d))
(cdr '((a b) c d))
;; 在lisp中，eval的方式是先计算内层再计算外层，这个我们通常接触的递归差不多
(second '(a b c d e))
(first '(a b c d e))
(listp '(a b)) ;; 其中的p表示predicate(谓词)，真为T，假为NIL
(null '())
(null nil)
(not '())
(not nil)
(if (listp '( 3 4))
	(+ 1 2)
	(- 3 2))

(if (= 1 1)
	(* 2 3)
	(/ 8 4))
(and nil t) ;; and 若全为真，则返回最后一个值，否则返回NIL
(or nil (- 2 1) t) ;; or 返回第一个实参为真的值，若全为假，则返回NIL
(car (cdr '(a b)))
(format t "~A plus ~A equals ~A. ~%" 2 3 (+ 2 7)) ;; format 函数是用于输出的，同时输出结果以及format函数的返回值，第一个实参t是表示输出到缺省的地方，第二个实参-字符串-为输出模板，其中的~A为占位符，~%表示换行
#|
输入函数是read，输出函数是format
其中read函数很强大
|#
(let ((x 4))
	(print x))

(defun ask-num ()
	(format t "Plz input number.")
	(let ((num (read))) ;; 从输入中读取字符串给指定的变量
		(if (numberp num)
			num
			(ask-num))))
(ask-num)


#|
let 用于设置局部变量
defparameter 用于设置全局变量
defconstant 用于设置全局常量
setf 用于变量赋值，若是该符号不存在，则隐式创建全局变量(最好不使用该方法创建全局变量)
|#
(let ((x 12))) ;; let 的实参是有一组键值对组成
(defparameter tabuyos 99)
(defconstant tabuyos 99)
(setf tabuyos 1) ;; 可以同时设置多个值
(print "Tabuyos")
(progn
	(print 1)
	(print 2)
	(print 3)) ;; progn 表示只返回最后一个表达式的值

(defun show-squares (i end)
   (if (> i end)
     'done
     (progn
       (format t "~A ~A~%" i (* i i))
       (show-squares (+ i 1) end))))
#|
# 表示function
' 表示quote
其中 #' 表示为升引号(sharp-quote)
apply 表示可以接受函数作为参数进行传递，最后一个实参必须是列表
funcall 和apply功能一样，但是最后一个实参不必是列表
|#
(apply #'+ '(1 2 3 4)) ;; 等价于(+ 1 2 3 4)
(apply #'+ 1 2 3 '(4)) ;; apply 接受任意数量的实参，但最后一个必须是列表
(funcall #'+ 1 2 3 4)
(funcall #'< 1 2)

;;;; Lambda 是symbol
(lambda (x) (+ x 100)) ;; 二者是等价的，Lisp程序员习惯加上lambda

;; lambda 还能匿名的创建函数
(lambda (x y)
	(+ 1 2))
((lambda (x) (+ x 100)) 1)
(funcall #'(lambda (x) (+ x 100)) 1)

(typep 27 'integer)
(typep (list 'a 'b) 'integer)
(typep (list 'a 'b) 'list)
(consp (cons 'a '(b c)))
(consp '(a b c))
(consp 'a) ;; 是atom
(consp (list 'a)) ;; 不是atom，是Cons
#|
所谓的Cons，指定是有两个部分(一对指针)
如同链表一样，一个指向值，另一个指向下一个所在地址

其中NIL既是一个atom，又是一个list

|#

(eql (cons 'a nil) (cons 'a nil)) ;; 调用一次cons的时候，lisp会重新分配内存地址给两个指针，eql比较的是地址
(equal (cons 'a nil) (cons 'a nil)) ;; equal 比较的是两个列表是否有相同的元素(非列表结构也同样有效)
(setf x '(a b c))
(setf y x)
(eql x y) ;; setf 是将地址赋给变量，因此相等，即复制指针

;; Lisp没有指针的原因是，每个值在概念上都是一个指针

#|
copy-list 用于复制list
copy-tree 用于复制tree
append 拼接任意数量的list
|#

(setf x '(T a b u y o s)
	y (copy-list x))
(atom y)
(append '(a b) '(c d) 'e)
(atom (cdr (cdr (cdr (cdr (append '(a b) '(c d) 'e))))))
(setf x '(a a b)) ;; a 为同一个值，即地址相同
(eql (car x) (car (cdr x)))

;; 一个简单的压缩算法
(defun compress (x)
	(if (consp x)
		(compr (car x) 1 (cdr x))
		x))
(defun compr (elt n lst)
	(if (null lst)
		(list (n-elts elt n))
		(let ((next (car lst)))
			(if (eql next elt)
				(compr elt (+ n 1) (cdr lst))
				(cons (n-elts elt n)
					(compr next 1 (cdr lst)))))))
(defun n-elts (elt n)
	(if (> n 1)
		(list n elt)
		elt))

;; 一个简单的解压算法
(defun uncompress (lst)
	(if (null lst)
		nil
		(let ((elt (car lst)) (rest (uncompress (cdr lst))))
			(if (consp elt)
				(append (apply #'list-of elt) rest)
				(cons elt rest)))))
(defun list-of (n elt)
	(if (zerop n)
		nil
		(cons elt (list-of (- n 1) elt))))

(compress '(1 1 1 1 1 1 2 3 0 1 1 0 0 0 0 3 3))
(uncompress '((3 1) 9 3 (5 3) 2 (2 1)))

#|
nth 用于返回第n+1个对象
nthcdr 用于返回第n个cdr的list
last 用于返回最后一个对象的list
同时定义了first-tenth
|#

(nth 4 '(a b c d e f g))
(nthcdr 4 '(a b c d e f g))
(last '(a b c d e f g))
(third '(a b c d e f g))
(cadddr '(a b c d e f g))

;; mapcar 用于将函数应用于多个列表
(mapcar #'list '(a b c) '(d e f) '(1 2 3 4))
(mapcar #'+ '(1 2 3) '(1 2 4))
(mapcar #'(lambda (x) (+ x 1)) '(1 2 3 4))

(copy-list '((b a) c d))
(copy-tree '((b a) c d))

;; 思考？为什么copy-tree和copy-list有何区别？？？
(defun our-copy-tree (tr)
  (if (atom tr)
       tr
       (cons (car tr)
		   (our-copy-tree (cdr tr)))))
(our-copy-tree '((b a) c d))

#|
substitute 替换的是序列中的元素但不含子序列中的元素
subst 替换的是序列中元素且包含子序列中的元素
|#

(subst 'y 'x '(and (integerp x) (zerop (mod x 2))))
(substitute 'y 'x '(and (integerp x) (zerop (mod x 2))))
(substitute 'y 'x '(and (integerp x) (zerop (mod x 2)) x))

(atom (car '(a b)))

#|
如何去判断递归函数是否是正确的，只需要查看递归函数中是否包含了所有的情况
|#

;; member 已知接受test key等关键字参数
(member '(a) '((a) (z)) :test #'equal)
(eql 'a 'a)
(function eql)

;; adjoin 类似于cons ，不同的是，当元素成员不在列表中才进行添加
;; adjoin 关键字参数同member
(adjoin 'a '(a b c))
(adjoin 'd '(a b c))

#|
集合中没有顺序概念
union 并集
intersection 交集
set-difference 补集
|#
(union '(a b c) '(x y z b))
(intersection '(a b c) '(x y z b))
(set-difference '(a b c d) '(b d))
(length '(a b c d)) ;; 返回序列中元素的个数
(subseq '(a b c) 1 2) ;; 第二个参数代表序列的开始位置(包含),第三个参数代表序列的结束位置(不包含),若是省略第三个参数,则是表示直到序列的末尾.
(reverse '(a b c d e)) ;; 将序列进行转置
(sort '(3 2 1 5 0 7 8 6) #'>) ;; sort 函数用于对序列进行排序
(every #'oddp '(1 3 5 7)) ;; 类似于全称量词
(some #'evenp '(1 2 3 5)) ;; 类似于存在量词
#|
push 入栈
pop 出栈
其中的push和pop均是由setf实现的函数
|#
(setf lst (list 'a))
(push 'b lst)
(push 'c lst)
(pop lst)

;; pushnew和push的不同点是,push使用的cons来添加元素,而pushnew则是使用的adjoin
(let ((x '(a b)))
	(pushnew 'c x)
	(pushnew 'a x)
	x)
(setf p (cons 'a 'b))
'(a . (b . (c . nil)))
(cons 'b 'c)
(setf test (cons 'a (cons 'b (cons 'c 'd))))
(cdddr test)

;; 一下四种形式均表示(a b)
'(a b . nil)
'(a . (b . nil))
'(a . (b))
'(a b)
;; 关联列表,速度较慢,类似于键值对
(setf trans '((1 . "add") (2 . "subtract")))
;; assoc 用于取出给定的键值有关联的Cons对
(assoc '1 trans) ;; 若是不存在,则返回NIL
#|
assoc 同member一样,接受关键字参数
assoc-if 传递的函数,是用于判断键的,当满足条件的键,均被输出
|#
(member-if #'oddp '(2 3 4)) ;; member-if 用于接受任意判断式
(assoc-if #'oddp trans)
(setf mins '((a b c) (b c) (c d)))
(assoc 'e mins)
(assoc 'a (list '(a b c) '(b c) '(c d)))
(list (list 'a))
;; 使用广度优先搜索
(defun shortest-path (start end net)
	(bfs end (list (list start)) net))

(defun bfs (end queue net)
	(if (null queue) ;; queue为空,则直接返回
		nil
		(let ((path (car queue))) ;; 从queue中取第一个元素并赋值给path
			(let ((node (car path)))
				(if (eql node end)
					(reverse path)
					(bfs end (append (cdr queue) (new-paths path node net)) net))))))
(defun new-paths (path node net)
	(mapcar #'(lambda (n)
				  (cons n path))
		(cdr (assoc node net))))
;; 按照广度优先搜索
(shortest-path 'a 'd mins)

#|
GC 垃圾回收机制
因为我们没有任何方法再存取列表，它也有可能是不存在的。
我们不再有任何方式可以存取的对象叫做垃圾。
你不用显式地配置 (allocate)或释放 (dellocate)內存
|#


;; 特殊的数据结构
;; 数组 Array
(setf arr (make-array '(2 3) :initial-element nil)) ;; 使用make-array生成一个指定的数组.
(aref arr 0 0) ;; 使用aref取出数组中的元素
;; 替换数组中的某个元素,可以使用setf+aref的方法
(setf (aref arr 0 0) '1)
(print arr) ;; #2A 表示字面常量的数组,语法是 #nA 其中n是数组的维度
(setf *print-array* t)

(setf vec (make-array 4 :initial-element nil)) ;; 若是第一个参数为一个整数,而不是一个列表,则创建一维数组(向量)
(vector "a" 'b 3) ;; 通过调用vector 填充向量, #表示字面常量向量
(setf (aref vec 3) '3) ;; 可以使用aref 取向量
(setf (svref vec 2) '2) ;; 可以使用svref 取向量(专门来取向量的,速度比aref快),sv表示simple vector
(print vec)
(length vec)
#|
如何对一个已经排好序的向量进行快速搜索?
我们可以使用二叉搜索算法
|#
(defun bin-search (obj vec)
	(let ((len (length vec)))
		(and (not (zerop len))
			(finder obj vec 0 (- len 1)))))
(defun finder (obj vec start end)
	(format t "~A~%" (subseq vec start (+ end 1)))
	(let ((range (- end start)))
		(if (zerop range)
			(if (eql obj (aref vec start))
				obj
				nil)
			(let ((mid (+ start (round (/ range 2))))) ;; round 返回离参数最近的整数
				(let ((obj2 (aref vec mid)))
					(if (< obj obj2)
						(finder obj vec start (- mid 1))
						(if (> obj obj2)
							(finder obj vec (+ mid 1) end)
							obj)))))))
(bin-search 3 vec) ;; 若是找到了该元素,则返回它,否则返回nil
(bin-search 3 #(0 1 2 3 4 5 6 7 8 9))
(+ 2 (round (/ 5 3)))
(sort "elbow" #'char<)
(char-code #\A) ;; 返回与字符相关的ASCII码
(code-char 98) ;; 返回与ASCII码相关的字符
(aref "abcd" 2) ;; 字符表示的语法是#\,如字符c表示为#\c
(char "abcd" 2) ;; 字符串可以使用char来进行取元素(速度比aref快)
(let ((str (copy-seq "Merlin")))
	(setf (char str 3) #\K)
	str)
(equal "friend" "fRiend") ;; 不忽略大小写
(string-equal "friend" "fRiend") ;; 忽略大小写
(print "Below")
(print(format nil "~A or ~A" "Truth" "Dare")) ;; 使用format来创建字符串,将第一个参数设为nil来调用format使它返回一个原本会打印出来的字符串
(concatenate 'string "Not" "to Worry.中国") ;; 将数个字符串连接起来,第一个参数为接受的特定类型符号

#|
序列类型包含了列表和向量(因此也包含了字符串)
所以,有些用在列表的函数,实际上是序列函数
remove length subseq reverse sort every some
都可以用于其他种类的序列
|#

(some #'evenp '(1 2 3 5))
#|
四种用来取出序列元素的函数
用于列表的nth
用于向量的aref及svref
用于字符串的char
以及通用的elt

针对特定类型的序列,使用特定的存取函数会比较快
因此使用elt是没有意义的

列表仅允许顺序存取
向量允许随机存取
|#
(nth 2 (list 'a 'b 'c 'd))
(aref vec 2)
(svref vec 2)
(char "MerKin" 2)
(elt '(a b c d ) 2)
#|
标准关键字参数
:key 应用至每个元素的函数 identity
:test 作来比较的函数 eql
:from-end 若为真,反向工作 nil
:start 起始位置 0
:end 若有给定,结束位置 nil
|#

;;position 接受所有的关键字参数
(position #\a "fantasia") ;; 返回序列中找到的第一个元素的位置,若未找到则返回nil
(position #\a "fantasia" :start 3 :end 5)
(position #\a "fantasia" :from-end t)
(position 'a '((c d) (a b) (a)) :key #'car)
(list 'a '((c d) (a b) (a)))
(position 'a '((c d) (a b) a) :key #'car)
(position '(a b) '((a b) (c d)))
(position '(a b) '((a b) (c d)) :test #'equal)

;; 分词
(defun second-word (str)
	(let ((p (+ (position #\  str) 1)))
		(subseq str p (position #\  str :start p))))
(second-word "This is Test.")
(defun split-word (str)
	(format t "~A~%" str)
	(let ((p (position #\  str)))
		(if (null p)
			str
		    (progn
				(split-word (subseq str (+ p 1)))
				(list (subseq str 0 p))))))
(split-word "this is test function")
(length "function")
(null (position #\  "ffff"))
(format nil "~A ~A ~A" "this" "is" "function")
(append (list "this") (list "is") (list "test"))

(position-if #'oddp '(2 3 4 6)) ;; position-if 接受除:test外的所有关键字

#|
member-if
assoc-if
position-if
find-if
remove-if
|#
(find #\a "cat")
(find-if #'characterp "ham")

;; remove-duplicates 仅保留最后一次出现的字符,接受所有的关键字参数
(remove-duplicates "afdsjfjhhhfwssiifds")
;; reduce 把序列压缩成一个值,至少接收两个参数,一个函数一个序列,其中函数必须是接受两个实参的函数

#|
在最简单的情况下，一开始函数用序列前两个元素作为实参来调用，
之后接续的元素作为下次调用的第二个实参，
而上次返回的值作为下次调用的第一个实参。
最后调用最终返回的值作为 reduce 整个函数的返回值
|#
;; fn 不是内置函数,此处为举例
(reduce #'fn '(a b c d)) ;; 等价于下面
(fn (fn (fn 'a 'b) 'c) 'd)
;; 还可以利用reduce来进行参数扩充
(reduce #'intersection '((b r a d 's) (b a d) (c a t))) ;; 等价于下面
(intersection (intersection '(b r a 's) '(b a d)) '(c a t))

(defun tokens (str test start)
	(let ((p1 (position-if test str :start start)))
		(if p1
			(let ((p2 (position-if #'(lambda (c)
										 (not (funcall test c)))
						  str :start p1)))
				(cons (subseq str p1 p2) ;; 从第一个是字符到第一个不是字符的的子序列
					(if p2
						(tokens str test p2)
						nil)))
			nil)))
(defun constituent (c)
	(and (graphic-char-p c)
		(not (char= c #\ ))))
;; 只筛选a-zA-Z
(tokens "ab12 3cde.f" #'alpha-char-p 0)
(tokens "This is test function" #'alpha-char-p 0)

#|
graphic-char-p 指所有的字符
alpha-char-p 指字母
|#

(position-if #'alpha-char-p "12asd 2fds 23 sa") ;; 返回第一个字符的位置
(position-if #'(lambda (c) (not (funcall #'alpha-char-p c))) "12a df 3 fds23" :start 2)
(graphic-char-p #\ )
(funcall #'oddp 21)
;; 字符是的是a-zA-Z
(not (alpha-char-p #\a))
;; 按空格进行分割
(tokens "ab12 3cde.f gh" #'constituent 0)
(parse-integer "1232")
(dotimes (i 5)
	(print i))
(digit-char-p #\1)
(char "123" 1)
(defun read-int (str)
	(if (every #'digit-char-p str)
		(let ((ac 0))
			(print ac)
			(dotimes (pos (length str))
				(setf ac (+ (* ac 10) (digit-char-p (char str pos)))))
			ac) ;; 最后的ac是指的返回值.
		nil))
(read-int "12345")
(setf vec (make-array 5 :initial-element nil))


(defun block-height (b)
	(svref b 0))
(setf (svref vec 0) '12)
(block-height vec)


#|
定义结构体使用defstruct
|#
(defstruct point
	x
	y)
#|
定义了一个point的结构,具有两个字段
同时隐式的定义了make-point point-p copy-point point-x point-y等函数
Lisp程序可以写出Lisp程序,这是目前明显的例子之一
等我们调用defstruct的时候,他自动生成了其他几个函数的相关定义
有了宏以后你将可以自己来办到同样的事情(如果需要,你甚至可以自己写出defstruct)
|#

;; 生成一个point,使用make-point
(setf p (make-point :x 0 :y 1)) ;; 结构体表示的语法是#S
(point-p p) ;; 等价下面
(typep p 'point) ;; 通用的函数
(setf p1 (copy-point p))
(point-x p)
(point-y p)

;; 一个较复杂的结构
(defstruct polemic
	(type (progn
			  (format t "What kind of polemic was it? ")
			  (read)))
	(effect nil))
(make-polemic :type "Tabuyos" :effect 1)
(make-polemic)

(defstruct (point (:conc-name p)
			   (:print-function print-point))
	(x 0)
	(y 1))
(defun print-point (p stream depth)
	(format stream "#<~A, ~A>" (px p) (py p)))
#|
:conc-name 关键字参数指定了要放在字段前面的名字,并用这个名字生成存取的函数
预设是point-,现在变成了p
:print-function 是在需要显示结构出来看是,指定用来打印结构的函数
需要显示的情况比如:要在顶层显示时
这个函数接收三个实参:要被打印出来的结构,在哪里被打印出,第三个参数通常可以忽略
|#

(make-point)

;; BST 二叉搜索树
;; 当二叉搜索树平衡时,允许我们在与时间成log(n)比例的时间内,来增删查,其中n为集合大小
(defstruct (node (:print-function
					 (lambda (n s d)
						 (format s "#<~A>" (node-elt n)))))
	elt
	(l nil)
	(r nil))
(defun bst-insert (obj bst <)
	(if (null bst)
		(make-node :elt obj)
		(let ((elt (node-elt bst)))
			(if (eql obj elt)
				bst
				(if (funcall < obj elt)
					(make-node
						:elt elt
						:l (bst-insert obj (node-l bst) <)
						:r (node-r bst))
					(make-node
						:elt elt
						:r (bst-insert obj (node-r bst) <)
						:l (node-l bst)))))))
(defun bst-find (obj bst <)
	(if (null bst)
		nil
		(let ((elt (node-elt bst)))
			(if (eql obj elt)
				bst
				(if (funcall < obj elt)
					(bst-find obj (node-l bst) <)
					(bst-find obj (node-r bst) <))))))
(defun bst-min (bst)
	(and bst
		(or (bst-min (node-l bst)) bst)))
(defun bst-max (bst)
	(and bst
		(or (bst-max (node-r bst)) bst)))

(setf nums nil)
(dolist (x '(5 8 4 2 1 9 6 7 3))
	(setf nums (bst-insert x nums #'<))
	(format t "~A~%" nums))
(print nums)
(node-r (node-r nums))
(bst-find 12 nums #'<)
(bst-find 3 nums #'<)
(bst-min nums)
(bst-max nums)

(defun bst-remove (obj bst <)
	(if (null bst)
		nil
		(let ((elt (node-elt bst)))
			(if (eql obj elt)
				(percolate bst)
				(if (funcall < obj elt)
					(make-node
						:elt elt
						:l (bst-remove obj (node-l bst) <)
						:r (node-r bst))
					(make-node
						:elt elt
						:r (bst-remove obj (node-r bst) <)
						:l (node-l bst)))))))
(defun percolate (bst)
	(cond ((null (node-l bst))
			  (if (null (node-r bst))
				  nil
				  (rperc bst)))
		((null (node-r bst)) (lperc bst))
		(t (if (zerop (random 2))
			   (lperc bst)
			   (rperc bst)))))
(defun rperc (bst)
	(make-node
		:elt (node-elt (node-r bst))
		:l (node-l bst)
		:r (percolate (node-r bst))))
(defun lperc (bst)
	(make-node
		:elt (node-elt (node-l bst))
		:r (node-r bst)
		:l (percolate (node-l bst))))

;; 删除的节点,是用仅次于该节点的后代节点进行替换的,如
;; 2-3-7-9-10,当删除7时,可以用3替换,或者用9替换
(setf nums (bst-remove 2 nums #'<))
(bst-find 2 nums #'<)
(random 3) ;; 随机的生成三个数(从0开始)

;; bst 遍历
(defun bst-traverse (fn bst)
	(when bst
	(bst-traverse fn (node-l bst))
	(funcall fn (node-elt bst))
		(bst-traverse fn (node-r bst))))
(bst-traverse #'princ nums) ;; princ仅显示单一对象
(setf ht (make-hash-table)) ;; hash table 表示语法 # (和函数一样)
(gethash 'color ht) ;; 哈希表和关联列表类似,是一种表达对应关系的方式,使用gethash获取相关的数值,若不存在,则返回nil
;; gethash 返回了多个值,第一个值是与键值有关的数值,第二个值说明了哈希表是否含有任何用此键值来存储的数值,由于第二个值是nil因此第一个nil是表示缺省的值,而不是color对应的nil
(setf (gethash 'color ht) 'red)
(gethash 'color ht)
(setf bugs (make-hash-table))
(push "Doesn't take keyword arguments."
	(gethash #'member bugs))
;; 当集合变大时,哈希表的查询和移除会来得更快
(setf fruit (make-hash-table))
(setf (gethash 'apricot fruit) t) ;; 添加
(gethash 'apricot fruit) ;; 查询
(remhash 'apricot fruit) ;; 移除

;; 哈希表有个迭代函数 maphash 接受两个实参,即接受两个参数的函数和哈希表,该函数会被每个键值对调用,没有特定的顺序
(setf (gethash 'shape ht) 'spherical
	(gethash 'size ht) 'giant)
(maphash #'(lambda (k v)
			   (format t "~A = ~A~%" k v))
	ht) ;; maphash 总是返回nil,但你可以通过传入一个会累计数值的函数,把哈希表的词条存在列表里

(make-hash-table :size 5)


;; Chapter 5 Control Stream
#|
Common Lisp 有三个构造块基本操作符
progn 表达式会依次求值,并返回最后一个值,由于只返回最后一个表达式的值
所以使用progn带有副作用
block 就好像是一个带有名字和出口的progn,通过调用return-from突然且优雅的退出
之后的表达式不在被求值,当名字为nil时,可以使用return来返回
许多接受一个表达式主题的操作符,都隐含在一个叫做nil的区块中
tagbody 出现在主体的原子（atom）被解读为标签（labels)
把这样的标签传给 go ，会把控制权交给标签后的表达式
大多数的迭代操作符都隐含在一个tagbody
所以是可能可以在主体里使用标签及go
|#

(progn
	(print 1)
	(print 2)
	(print 3))
(block head
	(print "test")
	(return-from head "file")
	(print "this"))
(block nil
	(return 27))
(block nil
	(print 1)
	(return 33)
	(print 2))
;; 隐含在nil区块中
(dolist (x '(a b c d e))
	(format t "~A " x)
	(if (eql x 'c)
		(return 'done))) ;; return后程序退出
(dolist (x '(a b c d e))
	(format t "~A " x)
	(if (eql x 'c)
		x)) ;; 全部运行完
;; defun 定义的函数主体,都隐含了一个与函数同名的区块
(defun foo ()
	(return-from foo 33))
(foo)
;; 在一个显式或隐式的block外,不论是return-from或return,都不会工作
;; 使用return-from我们可以编写一个更好的read-integer
(defun read-integer (str)
	(let (( ac 0))
		(dotimes (pos (length str))
			(let ((i (digit-char-p (char str pos))))
				(if i
					(setf ac (+ (* ac 10) i))
					(return-from read-integer nil))))
		ac))
(read-integer "123456432")
(char "function" 2) ;; 返回指定位置的字符

(tagbody
	(setf x 0)
	top ;; 这个就是一个出现在主体的原子
	(setf x (1+ x))
	(format t "~A " x)
	(if (< x 10)
		(go top))) ;; 类似于C语言中goto
(setf x 0 y 1)
(print y)
(print x)

;; 进入一个 let 等同于执行一个函数调用
(let ((x 2) (y 7))
	(format t "~A" "Number")
	(+ x y))
((lambda (x y)
	 (format t "~A" "Number")
	 (+ x y))
	2 7)
;; 上述两种等价,因为,进入一个let等同于执行一个函数调用
;; 同时告诉我们let创造的变量的值,不能依赖其他由同一个let所创造的变量(外部不能用内部,或者实参不能用形参)
(let ((x 2) (y (+ x 1)))
	(+ x y))
((lambda (x y)
	 (+ x y))
	2 (+ x 1))
;; (+ x 1) 作为实参传给函数,不能引用函数内的形参x

;; 如果真的需要新变量的值,以来同一个表达式所设立的另一个变量?
;; 那么这个情况下,需要使用一个变形版本的let*
(let* ((x 1) (y (+ x 2)))
	(+ x y))
;; let* 等同于一个嵌套的let
(let ((x 1))
	(let ((y (+ x 2)))
		(+ x y)))
;; let和let*的初始值都是nil

;; destructuring-bind 是通用化的let,接受单一变量,
;; 一个模式(一个或多个变量所构成的树),并将它们与某个实际的树所对应的部分做绑定
;; 若给定的树(第二个参数)没有与模式匹配(第一个参数)时,会产生错误
(destructuring-bind (w (x y) . z) '(a (b c) d e f)
	(list w x y z))

(when (oddp that)
	(format t "Hmm, that's odd.")
	(+ that 1)) ;; 等价下面
(if (oddp that)
	(progn
		(format t "Hmm, that's odd.")
		(+ that 1)))
;; when的相反是unless,接受相同的参数,但是测试是返回为假时才对主体求值
(unless (evenp that)
	(format t "Hmm, that's odd.")
	(+ that 1))
;; cond 的两个新的优点,允许多个条件判断,与每个条件相关的代码隐含在progn中
;; cond 预期在我们需要使用嵌套if的情况下使用
(defun our-member (obj lst)
	(if (atom lst)
		nil
		(if (eql (car lst) obj)
			lst
			(our-member obj (cdr lst))))) ;; 等同于下面
(defun our-member (obj lst)
	(cond ((atom lst) nil)
		((eql (car lst) obj) lst)
		(t (our-member obj (cdr lst))))) ;; 最后一个总会执行(如果前面的一个都没有执行的话)
;; cond 表达式被求值时,测试条件式依次求值,直到某个测试条件式为真才停止,同时与其关联的表达式也会被求值,最后一个返回的数值,会作为cond的返回值
;; 若是cond的条件式没有表达式,则会返回条件式的值
(cond (99))

;; 一个数值与多个常量进行比较
(defun month-length (mon)
	(case mon
		((jan mar may jul aug oct dec) 31)
		((apr jun sept nov) 30)
		(feb (if (leap-year) 29 28))
		(otherwise "Unknown Month")))
(case 99 (99)) ;; 缺省子句的键值可以试t或者otherwise,若是没有子句符合或者子句字包含键值时,则返回nil
(case 99 ((99) 1))
;; typecase与case相似


#|
不是很明白do
|#
(defun show-squares (start end)
	(do ((i start (+ i 1)))
		((> i end) 'done)
		(format t "~A ~A~%" i (* i i))))
(show-squares 1 6)
(let ((x 'a))
	(do ((x 1 (+ x 1))
		(y x x))
		((> x 5))
		(format t "(~A ~A) " x y)))
(do ((x 0 (+ x 1))) ((= x 5) 'done) (format t "~A " x))
(do ((x 1 (+ x 1))
		(y x x))
	((> x 5))
	(format t "(~A ~A) " x y))
(do* ((x 1 (+ x 1))
		(y x x))
	((> x 5))
	(format t "(~A ~A) " x y))

(dolist (x '(a b c d e f) 'done)
	(format t "~A " x))
(dotimes (x 5 x)
	(format t "~A " x))
(dotimes (x 5)
	(format t "~A " x))

;; 下面是一个无任何副作用的阶乘函数
(defun factorial (n)
	(do ((j n (- j 1)) ;; do的循环体,j每次获取前一次的值
			(f 1 (* j f))) ;; f用于存储,f每次获取前一次的值
		((= j 0) f))) ;; do的测试表达式,为真时返回f
(factorial 10) ;; 无副作用的阶乘函数
(mapc #'(lambda (x y)
			(format t "~A ~A " x y))
	'(hip flip slip)
	'(hop flop slop)) ;; mapc 总是返回第二个参数的值

#|
Multiple Values
可以同时返回多个值
若一个values表达式是函数主体最后求职的表达式,它所返回的数值变成函数返回值
|#
(values 'a nil t (+ 2 3))
((lambda (x y)
	 (values x y (+ x y))) 2 3)
((lambda () ((lambda () (values 1 2)))))
;; (lambda () (lambda () (values 1 2))) ;; 注意该写法的不同,为什么上述写法需要加两层括号

(let ((x (values 1 2 3))) x) ;; 若只接受一个值,那么剩下的值将会被全部的丢弃
(values) ;; 不带参数的values是没有返回值的
(let ((x (values))) x) ;; 若只接收一个值,那么不带参数的values会返回以nil

;; multiple-value-bind 用于接受多个值
(multiple-value-bind (x y z) (values 1 2 3)
	(list x y z))
(multiple-value-bind (x y z) (values 1 2)
	(list x y z))
(get-decoded-time)
;; 显示当前的时间
(multiple-value-bind (s m h) (get-decoded-time)
	(format t "~A:~A:~A" h m s))

;; multiple-value-list 用于创建list
(multiple-value-list (values 1 2 3 4)) ;; 等同于下面
(multiple-value-call #'list (values 1 2 3 4))

#|
使用catch和throw,将控制权进行转移,throw控制交给catch
catch表达式接受一个标签
catch 用于捕捉块内throw的内容
unwind-protect 确保控制权移交时,代码不会被打断
|#
(defun super ()
	(catch 'abort
		(sub) ;; 这里捕捉到了throw,因此程序直接中止了,标签是abort
		(format t "We'll never see this.")))
(defun sub ()
	(throw 'abort 99))
(super)
(progn
	(error "Oops!")
	(format t "After the error."))
(catch 'abort
	(unwind-protect ;; 防止代码被打断
		(throw 'abort 99)
		(setf x 2)))
x
(catch 'abort
	(throw 'abort 99)
	(setf x 3)) ;; 代码被throw打断了,造成了中止
x

(setf mon '(31 28 31 30 31 30 31 31 30 31 30 31))
(length mon)
(apply #'+ mon)
(setf nom (reverse mon))
(setf sums (maplist #'(lambda (x)
						  (apply #'+ x))
			   nom))
(maplist #'(lambda (x) (apply #'+ x)) '(1 2 3 4))
(+ 2 (cddddr '(1 2 3 4)))


;; 常量定义
(defconstant month
	#(0 31 59 90 120 151 181 212 243 273 304 334 365))
(defconstant yzero 2000)
(defun leap? (y)
	(and (zerop (mod y 4))
		(or (zerop (mod y 400))
			(not (zerop (mod y 100))))))

;; 转换日期至数字
(defun date->num (d m y)
	(+ (- d 1) (month-num m y) (year-num y)))
(defun month-num (m y)
	(+ (svref month (- m 1))
		(if (and (> m 2) (leap? y)) 1 0)))
(defun year-num (y)
	(let ((d 0))
		(if (>= y yzero)
			(dotimes (i (- y yzero) d)
				(incf d (year-days (+ yzero i))))
			(dotimes (i (- yzero y) (- d))
				(incf d (year-days (+ y i)))))))
(defun year-days (y)
	(if (leap? y)
		366
		365))
(dotimes (i 12 1) ;; 第三个参数是dotimes的返回值
	(format t "~A " i))
(mapcar #'leap? '(1904 1900 1600 2000))


;; 转换数字至日期
(defun num->date (n)
	(multiple-value-bind (y left) (num-year n)
		(multiple-value-bind (m d) (num-month left y)
			(values d m y))))
(defun num-year (n)
	(if (< n 0) ;; 下面的两个do* 需要仔细理解!(可参考前面的无副作用阶乘函数)
		(do* ((y (- yzero 1) (- y 1))
				 (d (- (year-days y)) (- d (year-days y))))
			((<= d n) (values y (- n d))))
		(do* (( y yzero (+ y 1))
				 (prev 0 d)
				 (d (year-days y) (+ d (year-days y))))
			((> d n) (values y (- n prev))))))
(defun num-month (n y)
	(if (leap? y)
		(cond ((= n 59) (values 2 29))
			((> n 59) (nmon (- n 1)))
			(t (nmon n)))
		(nmon n)))
(defun nmon (n)
	(let ((m (position n month :test #'<)))
		(values m (+ 1 (- n (svref month (- m 1)))))))
(defun date+ (d m y n)
	(num->date (+ (date->num d m y) n)))

(multiple-value-list (date+ 17 12 1997 60))

#|
fboundp 谓词将用来测试,是否有个函数与给定的函数绑定了
如果有个符号是函数的名字,则symbol-function会返回它
|#
(fboundp '+)
(symbol-function '+)
(setf (symbol-function 'add2)
	#'(lambda (x) (+ x 2)))
(fboundp 'add2)
(add2 1)
;; 实际上defun做了更多的工作,将其翻译成上面的setf表达式
;; 使用defun可以让程序看起来更加的简单,或多或少的帮助了编译器
;; 但严格说来,没有defun也能写程序
(defun add2 (x) (+ x 2))

#|
当函数定义,defun的第一个实参变成了一个列表(setf x)
即函数名是这种形式(setf x)的函数定义中,第一个实参代表新的数值
而剩余的实参代表了传给f的参数
|#

;; 不是很明白下面的定义的意思
(defun primo (lst) (cadr lst)) ;; 可以不必为了(setf primo)而定义primo,但通常这样的定义是成对的
(defun (setf primo) (val lst) ;; 定义了setf的第一个参数是primo的函数调用时,所执行的内容
	(setf (car lst) val))
(let ((x (list 'a 'b 'c 'd 'e)))
	(setf (primo x) 480) ;; 等同于 (setf (car x) 480)
	x)
(primo '(1 2 3 4))
(setf (primo '(1 2 3 4)) 480)
((setf primo) 480 '(1 2 3 4))

(let ((x (list 'a 'b 'c 'd 'e)))
	(setf (car x) 480)
	x)

#|
字符串也是Lisp的表达式,并且字符串没有任何的副作用,除非它是最后一个表达式
如果字符串成为了defun定义的函数主体的第一个表达式
那么这个字符会变成函数的文档字符串
使用 documentation 来获取文档字符串
|#
(defun foo (x)
	"This is foo function."
	x)
(documentation 'foo 'function) ;; 获取文档字符串
(foo 1)


#|
通过 defun 或者 symbol-function搭配setf 定义的函数是全局函数
如同全局变量一样
局部函数和局部变量一样
局部函数使用labels 来定义,它是一种像是给函数使用的let
它的第一个实参是一个新局部函数的定义列表,而不是变量规格说明列表
(name parameters . body)
|#
(labels (	;; 一个实参是新局部函数的定义列表
			(add10 (x) (+ 10 x))
			(consa (x) (cons 'a x)))
	(consa (add10 3)))

;; 定义一个递归的局部函数
(labels
	(
		(len (lst)
			(if (null lst)
				0
				(+ (len (cdr lst)) 1))))
	(len '(a b c d)))

;; let 可以被理解成函数调用,do也可以
(do
	(
		(x a (b x))
		(y c (d y)))
	(
		(test x y)
		(z x y))
	(f x y)) ;; 等同于下面
(labels
	(
		(rec (x y)
			(cond (
					  (test x y)
					  (z x y)
					  (t
						  (f x y)
						  (rec (b x) (d y)))))))
	(rec a c))
(do ((i 0 (+ 1 i))) ((= i 5) i) (print i))
(cond (nil 1) (t 2))

#|
参数列表
&rest 剩余参数,这个变量(&rest之后的所有变量)会被设成一个带有剩余参数的列表
&optional 选择性参数,所以普通的参数有时称为必要参数,缺省为nil,也可以指定缺省值
选择性参数的缺省值可以不是常量
可以是任何的 Lisp 表达式。若这个表达式不是常量
它会在每次需要用到缺省值时被重新求值
&key 关键字参数,是一种更灵活的选择性参数
如果把&key 放在形参列表,那么在&key 之后的形参都是选择性的
同时,参数位置不重要,而是符号标签 :
|#
(defun philosoph (thing &optional property)
	(list thing 'is property))
(philosoph 'death)

(defun philosoph (thing &optional (property 'fun))
	(list thing 'is property))
(philosoph 'death)
(defun keylist (a &key x y z)
	(list a x y z))
(keylist 1)
(keylist 1 :y 2)
(keylist 1 :y 2 :x 3)
(keylist 1 :y 2 :z 4 :x 3)
(keylist 1 2 3) ;; error

(defun our-funcall (fn &rest args)
	(format t "~A " args)
	(apply fn args))
(funcall #'+ 1 2 3 4)
(apply #'+ 1 2 3 4) ;; error
(apply #'+ 1 2 3 (4)) ;; error
(apply #'+ 1 2 3 '(4))
(apply #'+ (1 2 3 4)) ;; error
(apply #'+ '(1 2 3 4)) ;; 这是funcall的形式
(our-funcall #'+ 1 2 3 4)
(our-funcall #'+ 1 2 3 (4)) ;; error
(our-funcall #'+ 1 2 3 '(4)) ;; error
(our-funcall #'+ (1 2 3 4)) ;; error
(our-funcall #'+ '(1 2 3 4)) ;; error

;; adjoin的定义
(defun our-adjoin (obj lst &rest args)
	(if (apply #'member obj lst args) ;; args 一定会是list
		lst
		(cons obj lst)))
(adjoin 'a '(a b c d))
(adjoin 'e '(a b c d))
(our-adjoin 'a '(a b c d))
(our-adjoin 'e '(a b c d))

;; destructuring-bind 在通常情况下,每个模式(pattern)中作为第一个参数的子树
;; 可以与函数的参数列表一样复杂
(destructuring-bind ((&key w x) &rest y) '((:w 3 :x 1) 2 a)
	(list w x y))

#|
你不需要改变你的想法来配合语言，因为你可以改变语言来配合你的想法
要获得可重用软件的方法是，由下而上地写程序
|#

#|
;; Closures 闭包
当函数引用到外部定义的变量时，这外部定义的变量称为自由变量
(free variable)。函数引用到自由的词法变量时，称之为闭包(closure)
只要函数还存在，变量就必须一起存在
闭包结合了函数与环境(environment)
无论何时，当一个函数引用到周围词法环境的某个东西时，闭包就被隐式地创建出来了
|#
;; 函数当作返回值
(defun combiner (x)
	(typecase x
		(number #'+)
		(list #'append)
		(t #'list)))
(defun combine (&rest args)
	(apply (combiner (car args)) args))
(combine 2 3 4)
(combine '(a b) '(c d))
(combine 'a 'b 'c 'd 'e)

;; 函数定义在词法环境中(let 生成的词法环境,变量叫做词法变量),同样能够引用词法变量
;; 即便函数被作为一个值返回了，返回至词法变量被创建的上下文之外
(setf fn (let ((i 3))
			 #'(lambda (x) (+ x i))))
(funcall fn 3)

;; 隐式的创建
(defun add-to-list (num lst)
	(mapcar #'(lambda (x)
				  (+ x num))
		lst))
;; lambda 表达式里的变量num是自由的,所以像这样的情况,我们传递了一个闭包给mapcar
(add-to-list 2 '(1 2 3))

;; 下面这个是一个比较明显的闭包,n是自由的
(defun make-adder (n)
	#'(lambda (x)
		  (+ x n)))

(setf add3 (make-adder 3))
(funcall add3 3)
(setf add27 (make-adder 27))
(funcall add27 3)

;; 产生共享变量的多个闭包
(let ((counter 0))
	(defun reset ()
		(setf counter 0))
	(defun stamp()
		(setf counter (+ counter 1))))
(list (stamp) (stamp) (reset) (stamp))

;; complement 接受一个谓词,并返回谓词的补数
(mapcar (complement #'oddp) '(1 2 3 4 5 6))
(mapcar #'oddp '(1 2 3 4 5 6))

;; 利用闭包实现,f是自由的
(defun our-complement (f)
	#'(lambda (&rest args)
		  (not (apply f args))))
(mapcar (our-complement #'oddp) '(1 2 3 4 5 6))
(defun 3+ (x) ;; 3+ 而不是 +3
	(+ x 3))
(3+ 3)

#|
动态作用域
什么时候派上用场?通常用来 暂时 给某个全局变量赋值
局部变量和全局变量的区别
实际差别是词法作用域的词法变量与动态作用域的特别变量的区别
但这俩货几乎是没有区别的,因为局部变量几乎总是词法变量,而全局变量总是特别变量
|#

;; 在词法作用域下,一个符号引用到上下文中符号名字出现的地方
;; 局部变量缺省着词法作用域
;; 所以,如果我们在一个环境里定义一个函数,其中有一个变量叫做x
(let ((x 10))
	(defun foo ()
		x))
(let ((x 20)) (foo)) ;; 这个词法环境中的x不会影响到foo中的x,因为foo中的x,仅在它的词法作用域中有效
;; 无论foo被调用时有存在其他的x,主体内的x都会引用到那个变量

;; 在动态作用域中,我们在环境中函数被调用的地方寻找变量
;; 要使一个变量是动态作用域的,我们需要在任何它出现的上下文中声明它是special

(let ((x 10))
	(defun foo ()
		(declare (special x))
		x))
(let ((x 20))
	(declare (special x))
	(foo))

;; 隐式创建使代码看起来混乱不堪,建议使用defparameter来取代,这个可以使程序看起来更简洁
(setf x 30) ;; 隐式创建
(foo)
;; 可以发现,上述的x只是暂时的被赋值了
*print-base* ;; 控制数字的显示方式,缺省10(即十进制)

;; 多个闭包
(let ((i 7))
	(defun print1 () (print i))
	(defun print2 () (print (+ i 1)))
	(defun reset0 () (setf i 1))
	(defun add10 () (setf i (+ i 10))))
(print1)
(print2)
(reset0)
(add10)


#|
编译,可以通过compiled-function-p来检查函数是否被编译
|#
(defun foos () (print 1)) ;; 许多实现会创建一个直译的函数
(compile 'foos) ;; 便衣这个函数,则直译的函数会被编译出来的取代
(compiled-function-p #'foos)

;; 一个像是 stamp 或是 reset 这种，在顶层明确使用词法上下文输入的函数 (即 let )
;; 不能作为参数传递给compile

;; 编译Lisp不是一个一个函数的去编译,而是使用compile-file编译整个文件
;; 这个函数接受一个文件名,并创建一个原始的编译版本(通常是同样的名称,不同的扩展名)
;; 编译过的文件被载入时,compiled-function-p会返回真
;; 外部函数被编译了,内部函数同样会被编译

#|
如何使用递归来解决问题?
你必须要示范如何解决问题的一般情况，通过将问题切分成有限小并更小的子问题。
你必须要示范如何通过 ── 有限的步骤，来解决最小的问题 ── 基本用例。

最显而易见的递归算法，不一定是最有效的。经典的例子是费氏函数

|#
;; 迭代版本的费氏函数
(defun fib (n)
	(do ((i n (- i 1))
			(f1 1 (+ f1 f2))
			(f2 1 f1))
		((<= i 1) f1)))
(fib 10)


#|
顶层输入输出缺省
*standard-input* 输入缺省
*standard-output* 输出缺省
路径名是一种指定一个文件的可移植方式
路径名包含了六个部分:host device directory name type version
可以通过make-pathname搭配一个或多个对应的关键字参数来产生一个路径
最简单的是只指定名字(name),让其他部分全部缺省
|#
(format nil "fdsfdsfs")
(format t "fdsfdsfs")
(setf path (make-pathname :name "myFile")) ;; 路径表示语法 #p


;; 开启一个文件的基本函数是open,它接受路径名以及大量选择性关键字参数
;; 若是开启成功,则返回一个指向文件的流
;; 我们可以在创建流时,指定我们想要如何使用它
;; 不论是写入流还是从流中读取或者同时进行读取,都可以通过direction参数设置
;; 三个对应的数值是:input :output :io
;; 如果是用来输出流的,if-exists参数说明了如果文件已经存在时该怎么做
;; 通常它应该是:supersede(取代的意思)

;; 创建一个可以写在myFile文件的流
(setf str (open path :direction :output :if-exists :supersede))
;; 将流作为参数传递给format(将会在流中打印出,而不是缺省),输入到流中(写入)
(format str "Sometimes~%")
;; 此时检查文件,可能有输出,也可能没有
;; 原因是某些实现会将输出累积成一块(chunks)再输出,直到我们将流关闭
(close str)
;; 使用完流后,永远要记得关闭流,在未关闭前,内容是不保证会出现的

;; 创建一个从文件读取的流
(setf str (open path :direction :input))
;; 从流中输出(读取)
(read-line str)
;; 关闭流
(close str)

;; 通常不直接使用open和close来操作IO
;; 使用with-open-file宏,通常更加方便,总是会关闭流
(with-open-file (str path :direction :output :if-exists :supersede)
	(format str "This is test file.~%"))


(progn
	(format t "Plz type your name: ")
	(read-line))

;; read-line 接受四个选择性参数:
;; 一个流
;; 一个参数用来决定遇到end-of-file时,是否产生错误
;; 若前一个为nil时,该返回什么
;; 第四个参数通常可以省略
(defun pseudo-cat (file)
	(with-open-file (str file :direction :input)
		(do ((line (read-line str nil 'eof)
				 (read-line str nil 'eof)))
			((eql line 'eof) 'end)
			(format t "~A~%" line))))
(pseudo-cat path)

;; 这个函数读取一个表达式,在表达式结束时停止读取
;; 所以读取或多余或少于一行,当然,必须是合法的Lisp语法
(read)

;; 该函数接受一个字符串,并返回第一个读取的表达式
;; 他同时返回两个值,第二个值指出停止的位置
(read-from-string "a b c")
(read-from-string "(a b c)")
(read-from-string '(a b c)) ;; error

(read-char)
(peek-char) ;; 和read-char类似,但是不会讲字符从流中移除

#|
三个最简单的输出函数是 prin1 , princ 以及 terpri 
这三个函数的最后一个参数皆为选择性的流参数，缺省是 *standard-output* 
|#

(prin1 "Hello World.") ;; 给程序读的
(princ "Hello World.") ;; 给人类读的
(terpri) ;; 打印空行
(documentation 'prin1 'function)

#|
输出函数 format 几乎可以用在所有的输出
它接受一个流,一个格式化字符串以及零个或多个参数
使用t作为第一个参数,则输出被送至*standard-output*
使用nil作为第一个参数,format则返回一个如何打印的字符串
|#

;; ~A 占位符像是使用的princ一般,~% 代表换行符
(format nil "Dear ~A, ~% Our records indicate..." "Mr. Malatesta")
;; ~S 格式化指令像~A, 但它使用prin1打印对象,而不是princ
(format t "~S ~A" "z" "z")
;; ~F 用来打印向右对齐的浮点数,可接受五个参数
;; 要打印出的字符总数(包含小数点),缺省是数字的长度
;; 小数点之后要打印几位数(即保留有效数字),缺省是全部
;; 小数点要往右移几位(即乘以几个数量级),缺省是没有
;; 若数字太长无法满足第一个参数时,所要打印出的字符,如果没有指定字符,一个过长的数字会尽可能使用他所需的空间被打印出
;; 数字开始打印之前左边的字符,缺省是空白

;; 下面是五个参数的例子
(format nil "~10,2,0,'*,' F" 2836.21875)
;; 不需要对应的参数则可以忽略
(format nil "~,2,,,F" 26.21875)
;; 省略一系列的逗号
(format nil "~,2F" 26.21875)

#|
Warnning!
当 format 取整数时，它不保证会向上进位或向下舍入。
就是说 (format nil "~,1F" 1.25) 可能会是 "1.2" 或 "1.3"
|#
(documentation 'format 'function)


;; 一个暂时储存输入的队列 (queue)称作缓冲区 (buffer)

;; 如何实现一个字符串匹配并替换的算法?


;; 一个宏字符或宏字符组合也称作read-macro(读取宏)
;; 许多CommonLisp的预定义的读取宏是缩写,如quote的缩写是'
;; 当读取一个像是'a的表达式的时候,它被读取器展开成(quote a)
(read-from-string "'a")
(cons (car (read-from-string "'a")) (cdr (read-from-string "'a")))
;; 所有预定义的派发读取宏是用井号作为派发字符(即第一个字符)
;; #' 表示(function ...)
;; ' 表示(quote ...)
;; 我们见过的派发读取宏包括
;; #(...) 产生一个向量
;; #nA(...) 产生数组
;; #\ 产生一个字符
;; #S(n ...) 产生一个结构
;; 当这些类型的每个对象被prin1显示时(或者format搭配~S)它们使用对应的读取宏
;; 这表示你可以写出或读回这样的对象
(let ((*print-array* t))
	(vectorp (read-from-string (format nil "~S" (vector 1 2 3)))))
;; 当然,我们拿到的不是同一个向量,而是具有同向元素的新向量
;; 不是所有的对象都能被显示时有着清楚(distinct),可读的形式
;; 例如,函数与哈希表倾向于这样#<...>被显示,实际上#<...>也是一个读取宏
;; 但是特别用来产生当遇到read的错误
;; 函数和哈希表不能被写出或读回,而这个读取宏正是为了确保使用者不会有这样的幻觉

#|
符号是变量的名字，符号本身以对象所存在
通过symbol-name 来获得符号的名字
缺省情况下,会把符号名字所有的英文字母转成大写,代表Common Lisp缺省不区分大小写
任何存在||(vertical bar)之间的字符序列被视为符号
|#
(eql 'abc 'AbC)
(car '(a b c))
(CaR '(a b c))

;; 当这种符号被读入时，不会有大小写转换，而宏字符与其他的字符被视为一般字符
(list '|Lisp 1.5| '|| '|abc| '|ABC|)

;; 如果 Lisp 没有用垂直杠表示一个符号
;; 如上述列表的最后一个，那么你也可以不用垂直杠
(symbol-name '|a b c|)
(symbol-name 'abc)
(symbol-name '|Lisp 1.5|)
;; 垂直杠是一种表示符号的特殊语法,他们不是符号的名字之一
;; 若是要在符号名称内使用垂直杠,则需要使用\(backslash)
(symbol-name '|a b \| c|)

#|
每个符号有一个属性列表,或称为plist
函数get 接受符号以及任何类型的键值,然后返回在符号的属性列表中与键值相关的数值
|#
(get 'alizarin 'color) ;; 它使用sql来比较各个键,没有找到则返回nil
;; 要将值与键关联起来时,你可以使用setf及get
(setf (get 'alizarin 'color) 'red)
(get 'alizarin 'color)

#|
符号的结构,使用symbol-可以查看具体的
name package value function plist
|#

(setf (get 'alizarin 'transparency) 'high)
;; 使用symbol-plist查看符号的属性列表
(symbol-plist 'alizarin)

#|
注意，属性列表不以关联列表（assoc-lists）的形式表示，虽然用起来感觉是一样的。
在 Common Lisp 里，属性列表用得不多。他们大部分被哈希表取代了
当两个变量设成相同的符号时，与两个变量设成相同列表一样：
两个变量的指针都指向同样的对象
|#

#|
包是将名字映射到符号的符号表(symbol-tables)。每个普通的符号都属于一个特定的包
符号属于某个包，我们称为符号被包扣押(intern)了
大多数的符号在读取时就被扣押了。
在第一次输入一个新符号的名字时，Lisp 会产生一个新的符号对象，
并将它扣押到当下的包里(缺省是 common-lisp-user 包)。
但也可以通过给入字符串与选择性包参数给 intern 函数，
来扣押一个名称为字符串名的符号

不是所有的符号都会被扣押。有时候有一个自由的（uninterned）符号是有用的
这和公用电话本是一样的原因。自由的符号叫做 gensyms 
|#
;; 将创建的符号扣押在RANDOM-SYMBOL包下面
;; 选择性包参数缺省是当前的包，所以前述的表达式，返回当前包里的一个符号，
;; 此符号的名字是 “RANDOM-SYMBOL”
;; 若此符号尚未存在时，会创建一个这样的符号出来。第二个返回值告诉我们符号是否存在
(intern "RANDOM-SYMBOL")

#|
如果程序的每个部分都是一个包，那么开发程序另一个部分的某个人
将可以使用符号来作为函数名或变量名，而不必担心名字在别的地方已经被用过了
假设一个程序分为两个包， math 与 disp 。
如果符号 fft 被 math 包导出，
则 disp 包里可以用 math:fft 来参照它。
在 math 包里，可以只用 fft 来参照
|#

;; 下面是你可能会放在文件最上方，包含独立包的代码：
(defpackage "MY-APPLICATION"
	(:use "COMMON-LISP" "MY-UTILITIES")
	(:nickname "APP")
	(:export "WIN" "LOSE" "DRAW"))
(in-package my-application)
#|
defpackage 定义一个新的包叫做 my-application 
它使用了其他两个包， common-lisp 与 my-utilities ，
这代表着可以不需要用包修饰符（package qualifiers）来存取这些包所导出的符号。
许多包都使用了 common-lisp 包 
因为你不会想给 Lisp 自带的操作符与变量再加上修饰符。

my-application 包本身只输出三个符号: WIN 、 LOSE 以及 DRAW 。
由于调用 defpackage 给了 my-application 一个匿称 app ，
则别的包可以这样引用到这些符号，比如 app:win
|#

#|
符号与特别变量有着直接的连接关系
符号与词法变量之间是没有连接的,只要一有值,符号就消失了
|#

;; 符号在概念上是原子性的,符号可以使用eql一步比较完成
;; 而字符串是需要使用string=或者string-equal逐一字符作比较

;; 演示一个随机文本
(defparameter *word* (make-hash-table :size 10000))
(defconstant maxword 100)
(defun read-text (pathname)
	(with-open-file (s pathname :direction :input)
		(let ((buffer (make-string maxword))(pos 0))
			(do ((c (read-char s nil :eof) (read-char s nil :eof)))
				((eql c :eof))
				(if (or (alpha-char-p c) (char= c #\'))
					(progn
						(setf (aref buffer pos) c)
						(incf pos))
					(progn
						(unless (zerop pos)
							(see (intern (string-downcase (subseq buffer 0 pos))))
							(setf pos 0))
						(let ((p (punc c)))
							(if p (see p)))))))))
(defun punc (c)
	(case c
		(#\. '|.|)
		(#\, '|,|)
		(#\; '|;|)
		(#\! '|!|)
		(#\? '|?|)))
(let ((prev '|.|))
	(defun see (symb)
		(let ((pair (assoc symb (gethash prev *word*))))
			(if (null pair)
				(push (cons symb 1) (gethash prev *word*))
				(incf (cdr pair))))
		(setf prev symb)))

;; read-text 接受一个路径名

(defun generate-text (n &optional (prev '|.|))
	(if (zerop n)
		(terpri)
		(let ((next (random-text prev)))
			(format t "~A " next)
			(generate-text (1- n) next))))
(defun random-text (prev)
	(let* ((choices (gethash prev *word*))
			  (i (random (reduce #'+ choices :key #'cdr))))
		(dolist (pair choices)
			(if (minusp (decf i (cdr pair)))
				(return (car pair))))))


;; 复数 a+bi 写成 #c(a b)
(+ 1 1)
(+ -1 2)
(+ 1.0 1)
(+ #c(0 1.0) 2)
(/ 3 2)
(+ #c(1.0 -1) #c(2 1.0)) ;; #C(3.0 0.0)
(+ #c(1 -1) #c(2 1)) ;; 3

;; Lisp 提供四种不同类型的数字的转换及取出位数的函数。
;; 函数 float 将任何实数转换成浮点数
(mapcar #'float '(1 2/3 0.5))
;; 函数 truncate 返回任何实数的整数部分
(truncate 1.3) ;; 会有 0.00000005 的误差是因为浮点数的计算本身就不精确
;; 函数 floor 与 ceiling 以及 round 也从它们的参数中导出整数
;; 使用 floor 返回小于等于其参数的最大整数
;; 而 ceiling 返回大于或等于其参数的最小整数
;; 函数 round 返回最接近其参数的整数

(defun our-truncate (n)
    (if (> n 0)
        (floor n)
        (ceiling n)))
(our-truncate 1.3)
(floor 1.3)
(ceiling 1.3)
(round 1.3)
(floor 1.5)
;; 函数 numerator 与 denominator 返回比值或整数的分子与分母
;; 函数 realpart 与 imgpart 返回任何数字的实数与虚数部分
;; 函数 random 接受一个整数或浮点数。这样形式的表达式 (random n)
;; 会返回一个大于等于 0 并小于 n 的数字，并有着与 n 相同的类型

;; zerop 、plusp 与 minusp 接受一个参数，分别于参数 = 、> 、< 零时，返回真
;; 只有 = 、 /= 与 zerop 可以用在复数
;; max 返回参数中最大值
;; min 返回参数中最小值

;; 宏 incf 及 decf 分别递增与递减数字
;; (incf x n) 类似于 (setf x (+ x n))
(/ 365 12)
(float (/ 365 12))

;; 指数 (expt n x)
;; 对数 (log x n) 自然对数 (log x)
;; 特殊指数 (exp x)
;; 三角函数 sin cos tan
;; 反三角函数 asin acos atan
;; 常量 most-positive-fixnum 与 most-negative-fixnum
;; 表示一个实现不使用大数所可表示的最大与最小的数字大小
most-positive-fixnum
most-negative-fixnum
;; 用十六个全局常量标明了每个格式的限制
;; 它们的名字是这种形式: m-s-f ，
;; 其中 m 是 most 或 least,s 是positive或negative,而 f 是四种浮点数之一



;; 如何跨越表达式与代码的界线?
(+ 1 2 3) ;; 等价于下面
(eval '(+ 1 2 3))
(format t "Hello World.") ;; 等价于下面
(eval '(format t "Hello World."))

(defun our-toplevel ()
  (do ()
      (nil)
    (format t "~%> ")
	  (print (eval (read)))))
(our-toplevel)

#|
1. 它的效率低下： eval 处理的是原始列表 (raw list)，
或者当下编译它，或者用直译器求值。两种方法都比执行编译过的代码来得慢许多。
2. 表达式在没有词法语境 (lexical context)的情况下被求值。
举例来说，如果你在一个 let 里调用 eval ，
传给 eval 的表达式将无法引用由 let 所设置的变量。
|#

#|
宏通常通过调用 defmacro 来定义
一个 defmacro 看起来很像 defun 
但是与其定义一个函数调用应该产生的值，它定义了该怎么翻译出一个函数调用
通过说明你一个调用应该要翻译成什么，来定义一个宏。这个翻译称为宏展开(由编译器自动完成)
|#

(defmacro nil! (x)
	(list 'setf x nil))
(nil! x) ;; 它接受一个参数。一个这样形式 (nil! a) 的调用，会在求值或编译前，被翻译成 (setf a nil)

;; 函数 macroexpand-1 接受一个宏调用，并产生它的展开式
(macroexpand-1 '(nil! x))

;; 一个宏调用可以展开成另一个宏调用
;; 当编译器（或顶层）遇到一个宏调用时，它持续展开它，直到不可展开为止

;; 一个反引号单独使用时，等于普通的引号
;; 反引号的优点是,在一个反引号表达式里,你可以使用,(逗号)与,@(comma-at)来重启求值
;; 如果你在反引号表达式里，在某个东西前面加逗号，则它会被求值。
;; 所以我们可以使用反引号与逗号来建构列表模版
(setf a 1 b 2)
'(a is a and b is b)
`(a is ,a and b is ,b) ;; 反引号表达式中,在符号前面使用,(逗号)可以重启求值

;; 因此可以通过反引号来取代list
(defmacro nil! (x)
	`(setf ,x nil))
(setf x 1)
x
(nil! x)
x

;; ,@ 和 , 相似,但将(本来应该是列表的)参数扒开,将列表的元素插入模板来取代列表
(setf lst '(a b c))
`(lst is ,lst)
`(Its elements are ,@lst)

(defmacro my-while (test &rest body)
	`(do ()
		 ((not ,test))
		 ,@body))

(let ((x 0))
	(my-while (< x 10)
		(print x)
		(incf x)))
(while)
(macroexpand-1 '(my-while (< x 10) (print x) (incf x)))
(let ((x 0))
	(do ()
		((not (< x 10)))
		(print x)
		(incf x)))

(defun quicksort (vec l r)
	(let ((i l)
			 (j r)
			 (p (svref vec (round (+ l r) 2))))    ; 1
		(my-while (<= i j)                           ; 2
			(my-while (< (svref vec i) p) (incf i))
			(my-while (> (svref vec j) p) (decf j))
			(when (<= i j)
				(rotatef (svref vec i) (svref vec j))
				(incf i)
				(decf j)))
		(if (>= (- j l) 1) (quicksort vec l j))    ; 3
		(if (>= (- r i) 1) (quicksort vec i r)))
	vec)

(quicksort #(2 5 1 8 4 9 0 3 7 6) 0 9)


;; 思考下面的两个程序是否存在问题
(ntimes 10
	(princ "."))
(defmacro ntimes (n &rest body)
	`(do ((x 0 (+ x 1)))
		 ((>= x ,n))
		 ,@body))
;; 上面的定义是否存在问题?

;; 若是在宏展开的时候,外部有个与宏展开内的变量有着相同的名字
;; 这会导致无法正确的引用宏展开内的变量
(let ((x 10))
	(ntimes 5
		(setf x (+ x 1)))
	x)
;; 上述的将结果是 10 ,与我们期望的 15 不同,原因如下
;; 上述的表达式被展开后如下
(let ((x 10))
	(do ((x 0 (+ x 1)))
		((>= x 5))
		(setf x (+ x 1)))
	x)
;; 可以发现上述中的do表达式中的x并没有作用到外部,因此外部的x始终没有发生改变
;; 程序误以为(setf x (+ x 1))中的x是do作用域中的x,实际上并不是

;; 普遍的解决方法是不要使用任何可能被捕捉的一般符号,而是使用gensym(自由的符号)
;; 因为 read 函数 intern(扣押) 每个它见到的符号，
;; 所以在一个程序里，不可能会有任何符号会 eql gensym

;; gensym 通常印出前面有 #: 的符号
;; 使用gensym重写ntimes,至少对于变量捕捉来说他是安全的
(defmacro ntimes (n &rest body)
	(let ((g (gensym)))
		`(do ((,g 0 (+ ,g 1)))
			 ((>= ,g ,n))
			 ,@body)))
(let ((x 10))
	(ntimes 5
		(setf x (+ x 1)))
	x) ;; 返回15,与预期相符

;; 但是这个宏又会出现新的问题:多重求值,g在每次迭代的时候都会求值
(let ((v 10))
	(ntimes (setf v (- v 1))
		(princ "."))) ;; 期望是 9 个句点,而不是 5 个
;; 我们期望的是下面这个
(let ((v 10))
	(ntimes 9
		(princ ".")))
;; 原因如下
;; (setf v (- v1))被传递进了do中,导致每次迭代,都会执行该表达式
(let ((v 10))
	(do ((#:g 0 (+ #:g 1)))
		((>= #:g (setf v (- v 1)))) ;; 可以发现(setf v (- v 1))在迭代中执行
		(princ ".")))

;; 解决方案:
;; 免非预期的多重求值的方法是设置一个变量
;; 在任何迭代前将其设为有疑惑的那个表达式。这通常牵扯到另一个 gensym

;; 正确的ntimes
(defmacro ntimes (n &rest body)
	(let ((g (gensym))
			 (h (gensym)))
		`(let ((,h ,n))
			 (do ((,g 0 (+ ,g 1)))
				 ((>= ,g ,h))
				 ,@body))))

(pprint (macroexpand-1 '(cond (a b) (c d e) (t f))))

(setf lst (list))
(print (push 1 lst))
(setf (car (push 1 lst)) (+ 2 (car (push 1 lst))))
lst

(define-modify-macro our-incf (&optional (y 1)) +)


;; 实用函数
(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
         (mapcar #'(lambda (expr)
                     `(,(incf key) ,expr))
                 exprs))))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))



#|
Common Lisp Object System 对象系统
defclass 定义一个类
defmethod 定义一个方法
(defclass colored-circle (circle colored)()) 类名中的参数叫做基类(如circle colored)
(defclass circle ()(radius)) 类下的成员叫做槽(slot)
当我们创造类的实例是,会继承基类(如果有的话),同时会获得,基类的槽
如果该类没有定义某个方法(即没有发生复写),则会使用某个基类的方法
其实,类与之前的结构很类似

传给defclass的第三个参数一定是必须是一个槽的定义列表
|#

(defclass circle ()
	(radius center))
;; 这个定义说明了circle类有着两个槽,分别叫做radius和center(槽类似于结构中的字段(field))

;; 要创建这个类,我们通过调用make-instance函数
(setf c (make-instance 'circle))
;; 给槽进行赋值
(setf (slot-value c 'radius) 1)
;; 我们可以对每个槽进行属性设置
(defclass circle ()
	((radius :accessor circle-radius)
		(center :accessor circle-center)))
;; 通过定义访问器(:accessor),如此一来我们就不必在调用slot-value
(setf (circle-radius c) 1)
(circle-radius c)
;; 通过定义:writer或者:reader而不是:accessor,我们可以设置访问器的读写行为
;; 指定一个缺省值,则是使用:initform参数
;; 若是想要在make-instance的时候直接将槽进行初始化,则需要指定:initarg
(defclass circle ()
	((radius :accessor circle-radius
		 :initarg :radius
		 :initform 10)
		(center :accessor circle-center
			:initarg :center
			:initform (cons 1 2))))
;; 创建时不指定槽的初始值,则使用缺省值
(setf c (make-instance 'circle))
;; 或者是在创建一个circle类的实例时,我们可以使用关键字参数:initarg 给槽赋值
(setf c (make-instance 'circle :radius 33))
(circle-radius c)
(circle-center c)

#|
Warnning
注意 initarg 的优先级比 initform 要高
|#


#|
1. 从网络的底部开始。
2. 往上走，遇到未探索的分支永远选最左边。
3. 如果你将进入一个节点，你发现此节点右边也有一条路同样进入该节点时，
   则从该节点退后，重走刚刚的老路，直到回到一个节点，
   这个节点上有尚未探索的路径。接着返回步骤 2。
4. 当你抵达表示 t 的节点时，遍历就结束了。
   你第一次进入每个节点的顺序就决定了节点在优先级列表的顺序。
|#

;; 通用函数
(defmethod combine (x y)
	(list x y))
(combine  3 4)

(defclass stuff () ((name :accessor name :initarg :name)))
(defclass ice-cream (stuff) ())
(defclass topping (stuff) ())

(defmethod combine ((ic ice-cream) (top topping))
	(format nil "~A ice-cream with ~A topping."
		(name ic)
		(name top)))
;; 上述的combine的参数被特化了:每个出现在列表的参数都有一个类别的名字

;; 一个方法的特化指出它是应用至何种类别的参数

;; 而当一个通用函数被调用时， Lisp 是怎么决定要用哪个方法的?
;; Lisp 会使用参数的类别与参数的特化匹配且优先级最高的方法。
;; 这表示若我们用 ice-cream 实例与 topping 实例去调用 combine 方法
;; 我们会得到我们刚刚定义的方法
(combine (make-instance 'ice-cream :name 'fig)
	(make-instance 'topping :name 'treacle))
;; 但使用其他参数时,我们会得到第一次定义的方法
(combine 23 'skip)
;; 因为第一个方法的两个参数皆没有特化，它永远只有最低优先权
;; 并永远是最后一个调用的方法。一个未特化的方法是一个安全手段
;; 就像 case 表达式中的 otherwise 子句

;; 在一个方法中任何的组合都可以特化
(defmethod combine ((ic ice-cream) x)
	(format nil "~A ice-cream with ~A" (name ic) x))
(combine (make-instance 'ice-cream :name 'Tabuyos) 23)

;; 如果没有可用的方法，我们会得到一个错误
;; 如果只有一个，它会被调用
;; 如果多于一个，最具体的会被调用

;; 不一定需要使用defclass层定义的类来做特化,使用其他类型依然可以做特化
(defmethod combine ((x number) (y number))
	(+ x y))
;; 甚至可以对单一的对象做特化,用eql来决定
(defmethod combine ((x (eql 'power)) (y (eql 'spark)))
	'boom)
;; 单一对象特化的优先级比类别特化来得高

;; 下列参数列表对是全部一致的
(x)             (a)
(x &optional y) (a &optional b)
(x y &rest z)   (a b &key c)
(x y &key z)    (a b &key c d)

;; 下列参数列表对不是一致的
(x)             (a b)
(x &optional y) (a &optional b c)
(x &optional y) (a &rest b)
(x &key x y)    (a)


#|
辅助方法
:before 在主体方法执行前执行
:after 在主体方法执行后执行
:around 若存在:around方法,则调用:around而不是主体方法,根据自身的判断,决定是否调用主体方法(通过call-next-method)
|#

(defclass speaker () ())
(defmethod speak ((s speaker) string)
	(format t "~A" string))
(defmethod speak :before ((s speaker) string)
	(princ "I think "))
(speak (make-instance 'speaker) "I'm Hungry")
(defclass intellectual (speaker) ())
(defmethod speak :before ((i intellectual) string)
	(princ "Perhaps "))
(defmethod speak :after ((i intellectual) string)
	(princ " in some sence"))
(speak (make-instance 'intellectual)
	"I am hungry")

;; 使用:around,使用call-next-method调用下个函数
;; 使用next-method-p检查是否有下个函数
(defclass courtier (speaker) ())
(defmethod speak :around ((c courtier) string)
	(format t "Does the King believe that ~A?" string)
	(if (eql (read) 'yes)
		(if (next-method-p)
			(call-next-method))
		(format t "Indeed, it is a preposterous idea. ~%"))
	'bow)

(speak (make-instance 'courtier) "Kings will last")

#|
在标准方法组合中，只有最具体的主方法会被调用
虽然它可以通过call-next-method来调用其它方法
但我们可能会想要把所有可用的主方法的结果汇总起来
|#
(defgeneric price (x)
	(:method-combination +))

(defclass jacket ()())
(defclass trousers ()())
(defclass suit (jacket trousers)())

(defmethod price + ((jk jacket)) 350)
(defmethod price + ((tr trousers)) 200)

(price (make-instance 'suit))

;; 下列函数可以用来作为defmethod的第二个参数或是作为defgeneric调用中method-combination的选项
;; + and append list max min nconc or progn
;; 你也可以使用standard,yields标准方法组合

#|
一旦你指定了通用函数要用何种方法组合，所有替该函数定义的方法必须用同样的机制。
而现在如果我们试着使用另个操作符（ :before 或 after ）
作为 defmethod 给 price 的第二个参数，则会抛出一个错误。
如果我们想要改变 price 的方法组合机制，
我们需要通过调用 fmakunbound 来移除整个通用函数
|#

;; 隐藏细节有时候被称为封装 (encapsulated)
;; 虽然封装通常与面向对象编程相关联,但这两个概念其实是没相干的,
;; 我们可以只需要拥有其一,而不需要另一个
;; 将下列进行封装
(defpackage "CTR"
  (:use "COMMON-LISP")
  (:export "COUNTER" "INCREMENT" "CLEAR"))

(in-package ctr)

(defclass counter () ((state :initform 0)))

(defmethod increment ((c counter))
  (incf (slot-value c 'state)))

(defmethod clear ((c counter))
  (setf (slot-value c 'state) 0))
;; 在包外部的代码只能够创造 counter 的实例
;; 并调用 increment 及 clear 方法，但不能够存取 state
;; 若是需要使用state,则需要解除扣押
(unintern 'state)

#|
有两种实现方式：消息传递模型 (message-passing model)与
通用函数模型 (generic function model)。
一开始先有的消息传递。通用函数是广义的消息传递

在消息传递模型里，方法属于对象，且方法的继承与槽的继承概念一样

功能上来说，消息传递模型是通用函数模型的子集
|#

;; 多个列表的共享结构
(setf part (list 'b 'c))
(setf whole (cons 'a part))
;; (B C) 被part和whole共享
;; 应努力避免共享结构,使用复制可以避免共享结构
;; 因此,当两个列表共享结构后,我们修改了其中一个,另一个也会随之改变
(setf whole (list 'a 'b 'c 'd) tail (cdr whole))
whole
tail
(setf (second tail) 'e)
;; 无论我们修改的是whole还是tail,我们修改的都是同一个cons
;; 一次修改两个对象并不总是错误的。有时候这可能正是你想要的
;; 当一个列表神秘的改变了的时候，很有可能是因为改变了其它与之共享结构的对象

#|
为了安全起见,干脆避免对结构使用setf(以及相关的运算,比如：pop rplaca等)
这样就不会遇到问题了。如果某些时候不得不修改列表结构时，
要搞清楚要修改的列表的来源，确保它不要和其它不需要改变的对象共享结构。
如果它和其它不需要改变的对象共享了结构，或者不能预测它的来源，
那么复制一个副本来进行改变。
|#

;; 破坏性函数
;; delete是remove的一个具有破坏性的版本
(setf lst '(a r a b i a))
(delete 'a lst)
lst

(setf lst (delete 'a lst))

(mapcar #'list
	'(a b c)
	'(1 2 3 4))

(mapcan #'list
	'(a b c)
	'(1 2 3 4))

;; nconc是append的一个具有破坏性的版本
;; mapcan的实现如下:
(defun our-mapcan (fn &rest lsts )
	(apply #'nconc (apply #'mapcar fn lsts)))

#|
优化的三点规则
1. 它应该关注瓶颈
2. 它不应该开始的太早
3. 它应该始于算法

在一个与 I/O 无关 (Non-I/O bound) 的程序中
大部分的运行时间集中在大概 3% 的源代码中

类型声明
全局声明以 declaim 伴随一个或多个声明的形式来实现
一个类型声明是一个列表，包含了符号 type ，后跟一个类型名
以及一个或多个变量组成,也可以省略

局部声明通过 declare 完成，它接受的参数和 declaim 的一样
|#
;; 全局声明
(declaim (type fixnum *count*))
(declaim (fixnum *count*))
;; 局部声明
(defun poly (a b x)
	(declare (fixnum a b x))
	(+ (* a (expt x 2)) (* b x)))
(poly 2 3 4)

;; 也可以通过 the 为某个表达式的值声明类型
;; 如果我们提前就知道 a 、 b 和 x 是足够小的定长数，并且它们的和也是定长数的话
(defun poly (a b x)
	(declare (fixnum a b x))
	(the fixnum (+ (the fixnum (* a (the fixnum (expt x 2))))
					(the fixnum (* b x)))))
;; 定义type,可以有效的提高效率(但不绝对),同时有可能减少空间的浪费

;; 可以通过 make-array 的 :element-type 参数指定数组包含值的种类
;; 这样的数组被称为特化数组(specialized array)
;; 一个特化的向量不再是一个简单向量,因此我们不再能够通过svref来引用它的元素
(setf a (make-array '(1000 1000)
			:element-type 'single-float
			:initial-element 1.0s0))

(defun sum-elts (a)
	(declare (type (simple-array single-float (1000 1000))
                 a))
	(let ((sum 0.0s0))
		(declare (type single-float sum))
		(dotimes (r 1000)
			(dotimes (c 1000)
				(incf sum (aref a r c))))
		sum))
(time (sum-elts a))
(declare (type (vector fixnum 20) v))
;; 类型声明的重要性 ── 特别是对数组和数来说 ── 怎么强调都不过分

#|
安全				破坏性
append				nconc
reverse				nreverse
remove				delete
remove-if			delete-if
remove-duplicates	delete-duplicates
subst				nsubst
subst-if			nsubst-if
union				nunion
intersection		nintersection
set-difference		nset-difference
|#

;; 减少构造的方法,有很多种,最简单的方法就是使用破坏性函数

;; 使用带有填充指针的向量有一个缺点，就是它们不再是简单向量了
;; 我们不得不使用 aref 来代替 svref 引用元素

;; 首先用Lisp来编写程序，然后用C改写它,要比从头开始就用C编写这个程序要好
(type-of 'integer)
(type-of 1)
;; 所有的对象皆为类型 t
;; 在层级的最顶端是类型 t
;; 从nil至顶端有两条路,举例来说:一条从atom,另一条从list与sequence
#|
复合类型标识符
如果a与b是两个类型标识符,则(or a b)表示分别由a与b类型所表示的联集(union)
也就是说,一个类型(or a b)的对象是类型a或类型b
|#

(simple-array fixnum (* *))

#|
如果有某些复合类型标识符你想重复使用，你可以使用 deftype 定义一个缩写
这个宏与 defmacro 相似，但会展开成一个类型标识符，而不是一个表达式
|#

(deftype proseq ()
	'(or vector (and list (not (satisfies circular?)))))

(typep #(1 2) 'proseq)

;; 如果你定义一个接受参数的类型标识符，参数会被视为 Lisp 形式(即没有被求值)
;; 与 defmacro 一样

#|
打开流时，通常是用 unsigned-byte ── 来作为 :element-type 的参数
关于二进制流的 I/O 函数仅有两个， read-byte 以及 write-byte
|#

;; 如何复制一个文件
(defun copy-file (from to)
	(with-open-file (in from :direction :input
						:element-type 'unsigned-byte)
		(with-open-file (out to :direction :output
                            :element-type 'unsigned-byte)
			(do ((i (read-byte in nil -1)
					 (read-byte in nil -1)))
				((minusp i))
				(declare (fixnum i))
				(write-byte i out)))))

;; Lisp 中最古老的读取宏之一是 ' ，即 quote
;; 函数 set-macro-character 提供了一种方式来定义读取宏 (read-macros)。
;; 它接受一个字符及一个函数，因此当read碰到该字符时，它返回调用传入函数后的结果
;; 定义如下
(set-macro-character #\'
	#'(lambda (stream char)
		  (list (quote quote) (read stream t nil t))))
(list (quote quote) 'a) ;; (QUOTE 'A)='A
(list (quote quote))

#|
read 函数接受的参数 (read &optional stream eof-error eof-value recursive)

现在我们明白了read最后一个参数的用途。它表示无论read调用是否在另一个read里
传给 read 的参数在几乎所有的读取宏里皆相同：
传入参数有流 (stream)；
接着是第二个参数,t,说明了read若读入的东西是end-of-file时，应不应该报错
第三个参数说明了不报错时要返回什么，因此在这里也就不重要了
而第四个参数 t 说明了这个 read 调用是递归的

你可以(通过使用make-dispatch-macro-character)来定义你自己的派发宏字符
(dispatching macro character),但由于 # 已经是一个宏字符，所以你也可以直接使用。
六个 # 打头的组合特别保留给你使用： #! 、 #? 、 ##[ 、 ##] 、 #{ 、 #} 

你可以通过调用 set-dispatch-macro-character 定义新的派发宏字符组合，
与 set-macro-character 类似，除了它接受两个字符参数外。
下面的代码定义了 #? 作为返回一个整数列表的读取宏
|#

(set-dispatch-macro-character #\# #\?
	#'(lambda (stream char1 char2)
		  (list 'quote
			  (let ((lst nil))
				  (dotimes (i (+ (read stream t nil t) 1))
					  (push i lst))
				  (nreverse lst)))))
#?7

#|
一个包是一个将名字映对到符号的Lisp对象。当前的包总是存在全局变量*package*里
当 Common Lisp 启动时，当前的包会是 *common-lisp-user* 
通常称为用户包 (user package)。函数 package-name 返回包的名字，
而 find-package 返回一个给定名称的包

通常一个符号在读入时就被 interned 至当前的包里面了。
函数 symbol-package 接受一个符号并返回该符号被 interned 的包
|#

(package-name *package*)
(find-package "COMMON-LISP-USER")
(symbol-package 'sym)

;; 使用其他包的符号common-lisp-user::sym
;; 前提是没有使用(in-package )

(in-package common-lisp-user)
;; 使用了(in-package )则只需要使用单引号即可

;; 通过把 bar 输入 ( import )至 mine 包，我们就能进一步让 mine 和 user 包可以共享 bar 这个符号
(import 'common-lisp-user:bar)

;; 另一个方法来获得别的包内符号的存取权是使用(use-package  )
;; 若是使用(use-package )则不需要使用任何的限定符即可使用

;; 最基本的捕捉错误
(error "Your report uses ~A as a verb." 'status)
;; 用来捕捉错误的更抽象操作符包括了 ecase 、 check-type 以及 assert
;; 前者与 case 相似，要是没有键值匹配时会捕捉一个错误
;; 或许会在当你没有 otherwise 子句时使用 ecase
;; assert 更加的通用
