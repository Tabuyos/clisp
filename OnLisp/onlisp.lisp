;; This is comment line.
;; The lisp file is the lesson file for practicing ccommon lisp --- reading <On Lisp> by Tabuyos

#|
	Chapter 1.
	Extensible language.
|#

;; Bottom-up Design
;; AI: Artificial Intelligence

#|
	Chapter 2.
	Functions.
|#

(defun double2 (x) (* x 2))

(double2 2)

#'double2

;; (defun double (x) (* x 2)) === (setf (symbol-function 'double) #'(lambda (x) (* x 2)))

(apply '+ '(1 2))
(apply #'+ '(1 2))

(funcall '+ 1 2)
(funcall #'+ 1 2)

(mapcar #'(lambda (x y) (+ x 10 y)) '(1 2 3) '(4 5 6)) ;; can't remove #
(mapcar #'+ '(1 2 3 4) '(4 5 6))
(mapcar '+ '(1 2 3 4) '(4 5 6))

(remove-if #'evenp '(1 2 3 4 5 6 7))

;; our function for remove-if
(defun our-remove-if (fn lst)
	(if (null lst)
		nil
		(if (funcall fn (car lst))
			(our-remove-if fn (cdr lst))
			(cons (car lst) (our-remove-if fn (cdr lst))))))

(our-remove-if #'evenp '(1 2 3 4 5 6 7))

(defun behave (animal)
	(case animal
		(dog (wag-tail)
			(bark))
		(rat (scurry)
			(squeak))
		(cat (rub-legs)
			(scratch-carpet))))

(behave 'dog)

(setf (get 'dog 'buhavior)
	#'(lambda ()
		  (wag-tail)
		  (bark)))

(defun behave (animal)
	(funcall (get animal 'behavior)))

(behave 'dog)

(defun list+ (lst n)
	(mapcar #'(lambda (x) (+ x n))
		lst))

(list+ '(1 2 3 4) 11)

(let ((y 7))
	(defun scope-test (x) (list x y)))

(scope-test 3)

(defun make-adder (n)
	#'(lambda (x) (+ x n)))

(setq add2 (make-adder 2)
	add10 (make-adder 10))

(add2 5) ;; error
(funcall add2 5)

(defun make-adderb (n)
	#'(lambda (x &optional change)
		  (if change
			  (setq n x)
			  (+ x n))))

(setq addx (make-adderb 1))

(funcall addx 3)

(funcall addx 100 t)

(defun make-dbms (db)
	(list
		#'(lambda (key)
			  (cdr (assoc key db)))
		#'(lambda (key val)
			  (push (cons key val) db)
			  key)
		#'(lambda (key)
			  (setf db (delete key db :key #'car))
			  key)))

(setq cities (make-dbms '((boston . us) (paris . france))))

(funcall (car cities) 'boston)
(funcall (second cities) 'london 'england)
(funcall (car cities) 'london)

(defun lookup (key db)
	(funcall (car db) key))

(labels ((inc (x) (1+ x)))
	(inc 3))

(defun count-instances (obj lsts)
	(labels ((instances-in (lst)
				 (format t "~A ~A~%" obj lst)
				 (if (consp lst)
					 (+ (if (eq (car lst) obj) 1 0)
						 (instances-in (cdr lst)))
					 0)))
		(mapcar #'instances-in lsts)))

(count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))

;; non-tail-recursive
(defun our-length (lst)
	(if (null lst)
		0
		(1+ (out-length (cdr lst)))))

(1+ (1+ (1+ 0)))

;; tail-recursive
(defun our-find-if (fn lst)
	(format t "~A~%" lst)
	(if (funcall fn (car lst))
		(car lst)
		(our-find-if fn (cdr lst))))

(our-find-if #'evenp '(1 3 4 5 6 7 8 9))

;; tail-recursive
(defun our-length (lst)
	(labels ((rec (lst acc)
				 (if (null lst)
					 acc
					 (rec (cdr lst) (1+ acc)))))
		(rec lst 0)))

(our-length '(1 2 3 4 5 3 2 2 2))

(defun triangle (n)
	(labels ((tri (c n)
				 (declare (type fixnum n c))
				 (if (zerop n)
					 c
					 (tri (the fixnum (+ c n))
						 (the fixnum (- n 1))))))
		(tri 0 n)))

(triangle 10000)

(compiled-function-p #'triangle)

(defun 50th (lst) (nth 49 lst))

(proclaim '(inline 50th))

(defun foo (lst)
	(+ (50th lst) 1))

#|
	Chapter 3.
	Functional programming.
|#

(setq lst '(a b c))

(defun bad-reverse (lst)
	(let* ((len (length lst))
			  (ilimit (truncate (/ len 2))))
		(do ((i 0 (1+ i))
				(j (1- len) (1- j)))
			((>= i ilimit))
			(rotatef (nth i lst) (nth j lst)))))

(bad-reverse lst)

lst

(do ((i 0 (1+ i)) (j 10 (1- j))) ((= i j)) (format t "i=~A j=~A~%" i j))

(defun good-reverse (lst)
	(labels ((rev (lst acc)
				 (if (null lst)
					 acc
					 (rev (cdr lst) (cons (car lst) acc)))))
		(rev lst nil)))

;; lst won't change
(good-reverse lst)

lst

(reverse lst)

(nreverse lst)

(truncate (/ 3 2))

(multiple-value-bind (int frac) (truncate (/ 3 2)) (list int frac))

(nth 3 '(a b c d e))

(nth 1 '(a b c d e))

(nth 8 '(a b c d e))

;; get multiple value by use values symbol
(defun powers (x)
	(values x (sqrt x) (expt x 2)))

(powers 4)

;; result
(defun fun (x)
	(list 'a (expt (car x) 2)))

(fun '(3 4))

;; processing
(defun imp (x)
	(let (y sqr)
		(setq y (car x))
		(setq sqr (expt y 2))
		(list 'a sqr)))

(imp '(3 4))

(defun qualify (expr)
	(nconc (copy-list expr) (list 'maybe)))

(qualify lst)

lst

(let ((x 0))
	(defun total (y)
		(incf y x)))

(total 9)

(let ((x 0)) 
	(defun total (y)
		(incf x y)))

(total 9)

(defun ok (x)
	(nconc (list 'a x) (list 'c)))

(ok 3)

(defun not-ok (x)
	(nconc (list 'a) x (list 'c)))

(not-ok '(4))

(defun anything (x)
	(+ x *anything*))

(anything 4)

(defun f (x)
	(let ((val (g x)))
		;; safe to modify val here?
		))

(defun exclaim (expression)
	(append expression '(on my)))

(exclaim '(lions and tigers and bears))

(nconc * '(goodness))

(exclaim '(fixnums and begnums and floats))

#|
	Chapter 4.
	Utility functions.
|#

(defun all-nicknames (names)
	(if (null names)
		nil
		(nconc (nicknames (car names))
			(all-nicknames (cdr names)))))

(mapcan #'nicknames people)

(let ((town (find-if #'bookshops towns)))
	(calues town (bookshops town)))

(defun find-books (towns)
	(if (null towns)
		nil
		(let ((shops (bookshops (car towns))))
			(if shops
				(values (car towns) shops)
				(find-books (cdr towns))))))

(defun find2 (fn lst)
	(if (null lst)
		nil
		(let ((val (funcall fn (car lst))))
			(if val
				(values (car lst) val)
				(find2 fn (cdr lst))))))

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
	(car (last lst)))

(defun single (lst)
	(and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
	(append lst (list obj)))

(defun conc1 (lst obj)
	(nconc lst (list obj)))

(defun mklist (obj)
	(if (listp obj) obj (list obj)))

(defun longer (x y)
	(labels ((compare (x y)
				 (and (consp x)
					 (or (null y)
						 (compare (cdr x) (cdr y))))))
		(if (and (listp x) (listp y))
			(compare x y)
			(> (length x) (length y)))))

(defun filter (fn lst)
	(let ((acc nil))
		(dolist (x lst)
			(let ((val (funcall fn x)))
				(if val (push val acc))))
		(nreverse acc)))

(defun group (source n)
	(if (zerop n) (error "zero length"))
	(labels ((rec (source acc)
				 (let ((rest (nthcdr n source)))
					 (if (consp rest)
						 (rec rest (cons (subseq source 0 n) acc))
						 (nreverse (cons source acc))))))
		(if source (rec source nil) nil)))

(last1 "blub") ;; error

(append1 lst 'd)

(consp lst)

(listp lst)

(conc1 lst 'd)

(group '(a b c d e f g h i j k l m n) 3)

(defun flatten (x)
	(labels ((rec (x acc)
				 (cond ((null x) acc)
					 ((atom x) (cons x acc))
					 (t (rec (car x) (rec (cdr x) acc))))))
		(rec x nil)))

(defun prune (test tree)
	(labels ((rec (tree acc)
				 (cond ((null tree) (nreverse acc))
					 ((consp (car tree))
						 (rec (cdr tree)
							 (cons (rec (car tree) nil) acc)))
					 (t (rec (cdr tree)
							(if (funcall test (car tree))
								acc
								(cons (car tree) acc)))))))
		(rec tree nil)))

(defun find2 (fn lst)
	(if (null lst)
		nil
		(let ((val (funcall fn (car lst))))
			(if val
				(values (car lst) val)
				(find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
	(and lst
		(let ((first (car lst)))
			(cond ((funcall test y first) nil)
				((funcall test x first) lst)
				(t (before x y (cdr lst) :test test))))))

(defun duplicate (obj lst &key (test #'eql))
	(member obj (cdr (member obj lst :test test))
		:test test))
(defun split-if (fn lst)
	(let ((acc nil))
		(do ((src lst (cdr src)))
			((or (null src) (funcall fn (car src)))
				(values (nreverse acc) src))
			(push (car src) acc))))

(before 'a 'b '(a d))

(duplicate 'a '(a b c a d))

(member 'b '(a b c d a d d e ) :test #'eql)

(split-if #'(lambda (x) (> x 4)) '(1 2 3 4 5 6 7 8 9 10))

(multiple-value-bind (first second) (split-if #'(lambda (x) (> x 4)) '(1 2 3 4 5 6 7 8 9 10)) (list first second))

(defun most (fn lst)
	(if (null lst)
		(values nil nil)
		(let* ((wins (car lst))
				  (max (funcall fn wins)))
			(dolist (obj (cdr lst))
				(let ((score (funcall fn obj)))
					(when (> score max)
						(setq wins obj max score))))
			(values wins max))))

(defun best (fn lst)
	(if (null lst)
		nul
		(let ((wins (car lst)))
			(dolist (obj (cdr lst))
				(if (funcall fn obj wins)
					(setq wins obj)))
			wins)))

(defun mostn (fn lst)
	(if (null lst)
		(values nil nil)
		(let ((rsult (list (car lst)))
				 (max (funcall fn (car lst))))
			(dolist (obj (cdr lst))
				(let ((score (funcall fn obj)))
					(cond ((> score max)
							  (setq max score result (list obj)))
						((= score max)
							(push obj result)))))
			(values (nreverse result) max))))

(best #'> '(1 2 3 4 9 6 7 9 8))

(mostn #'length '((a b) (a b c) (a d) (e f g h)))

(defun map0-n (fn n)
	(mapa-b fn 0 n))

(defun map1-n (fn n)
	(mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
	(do ((i a (+ i step))
			(result nil))
		((> i b) (nreverse result))
		(push (funcall fn i) result)))

(map0-n #'1+ 5)

(map1-n #'1+ 5)

(defun map-> (fn start test-fn succ-fn)
	(do ((i start (funcall succ-fn i))
			(result nil))
		((funcall test-fn i) (nreverse result))
		(push (funcall fn i) result)))

(do ((i 0 (1+ i))) ((= i 6)) (format t "~A~%" i))

(defun mapcars (fn &rest lsts)
	(let ((result nil))
		(dolist (lst lsts)
			(dolist (obj lst)
				(push (funcall fn obj) result)))
		(nreverse result)))

;; rmapcar: recursive mapcar
(defun rmapcar (fn &rest args)
	(if (some #'atom args)
		(apply fn args)
		(apply #'mapcar #'(lambda (&rest args)
							  (apply #'rmapcar fn args))
			args)))

(mapcars #'sqrt '(1 2 3) '(4 5 6 7) '(8) '(9))

(rmapcar #'sqrt '(1 2 3 (4 5 (6 7 8 (9)))))

(some #'atom '(1 (2)))

(defun readlist (&rest args)
	(values (read-from-string (concatenate 'string "(" (apply #'read-line args) ")"))))

(defun prompt (&rest args)
	(apply #'format *query-io* args)
	(read *query-io*))

(defun break-loop (fn quit &rest args)
	(format *query-io* "Entering break-loop. '~%")
	(loop
		(let ((in (apply #'prompt args)))
			(if (funcall quit in)
				(return)
				(format *query-io* "~A~%" (funcall fn in))))))

(readlist)

(values 1 2 3)

(prompt "Entering a number between ~A and ~A ~% >> "1 10)

(defun mkstr (&rest args)
	(with-output-to-string (s)
		(dolist (a args) (princ a s))))

(defun symb (&rest args)
	(values (intern (apply #'mkstr args))))

(defun reread (&rest args)
	(values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
	(map 'list #'(lambda (c)
					 (intern (make-string 1 :initial-element c)))
		(symbol-name sym)))

(mkstr pi " pieces of " 'pi)

(symb 'ar "Madi" #\L #\L 0)

(symb '(a b))

(let ((s (symb '(a b))))
	(and (eq s '|(A B)|) (eq s '\(A\ B\))))

(explode 'tabuyos)

(symbol-name 'tabuyos)

#|
	Chapter 5.
	Returning functions.
|#

(remove-if-not #'evenp '(1 2 3 4 5 6))

(remove-if (complement #'evenp) '(1 2 3 4 5 6))

(defun joiner (obj)
	(typecase obj
		(cons #'append)
		(number #'+)))

;; polymorphic -> join
(defun join (&rest args)
	(apply (joiner (car args)) args))

(join 1 2 3)

(defun make-adder (n)
	#'(lambda (x) (+ x n)))

(setq add3 (make-adder 3))

(funcall add3 2)

(defun my-complement (fn)
	#'(lambda (&rest args) (not (apply fn args))))

(remove-if (my-complement #'evenp) '(1 2 3 4 5 6))

(defvar *!equivs* (make-hash-table))

(defun ! (fn)
	(or (gethash fn *!equivs*)fn))

(defun def! (fn fn!)
	(setf (gethash fn *!equivs*) fn!))

(def! #'remove-if #'delete-if)

(setq lst '(1 2 3 4 5 6 7))

(delete-if #'oddp lst)

(funcall (! #'remove-if) #'oddp lst)

(defun memoize (fn)
	(let ((cache (make-hash-table :test #'equal)))
		#'(lambda (&rest args)
			  (multiple-value-bind (val win) (gethash args cache)
				  (if win
					  val
					  (setf (gethash args cache)
						  (apply fn args)))))))

(setq slowid (memoize #'(lambda (x) (sleep 5) x)))

(time (funcall slowid 1))

(time (funcall slowid 1))

(time (funcall slowid 7))

(time (funcall slowid 7))

(defun compose (&rest fns)
	(if fns
		(let ((fn1 (car (last fns)))
				 (fns (butlast fns)))
			#'(lambda (&rest args)
				  (reduce #'funcall fns
					  :from-end t
					  :initial-value (apply fn1 args))))
		#'identity))

(funcall (compose #'list #'1+) 1)

(funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))

(funcall (compose #'1+ #'find-if) #'oddp '(2 9 4))

(defun my-complement (pred)
	(compose #'not pred))

(mapcar #'(lambda (x)
			  (if (slave x)
				  (owner x)
				  (employer x)))
	people)

(defun fif (if then &optional else)
	#'(lambda (x)
		  (if (funcall if x)
			  (funcall then x)
			  (if else (funcall else x)))))

(defun fint (fn &rest fns)
	(if (null fns)
		fn
		(let ((chain (apply #'fint fns)))
			#'(lambda (x)
				  (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
	(if (null fns)
		fn
		(let ((chain (apply #'fun fns)))
			#'(lambda (x)
				  (or (funcall fn x) (funcall chain x))))))

(find-if #'(lambda (x)
			   (and (signed x) (sealed x) (delivered x)))
	docs)

;; equal
(find-if (fint #'signed #'sealed #'delivered) docs)

(defun our-length (lst)
	(if (null lst)
		0
		(1+ (our-length (cdr lst)))))

(defun our-every (fn lst)
	(if (null lst)
		t
		(and (funcall fn (car lst))
			(our-every fn (cdr lst)))))

(our-every #'oddp '(1 3 5 6))

(defun lrec (rec &optional base)
	(labels ((self (lst)
				 (if (null lst)
					 (if (functionp base)
						 (funcall base)
						 base)
					 (funcall rec (car lst)
						 #'(lambda ()
							   (self (cdr lst)))))))
		#'self))

;; lrec's first argument must be function that can be capture two arguments that car of list and function

;; our-length
(lrec #'(lambda (x f) (1+ (funcall f))) 0)

;; our-every
(lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)

(functionp t)

(functionp nil)

;; copy-list
(lrec #'(lambda (x f) (cons x (funcall f))))

;; remove-duplicates
(lrec #'(lambda (x f) (adjoin x (funcall f))))

;; find-if, for some function fn
(lrec #'(lambda (x f) (if (fn x) x (funcall f))))

;; some, for some function fn
(lrec #'(lambda (x f) (or (fn x) (funcall f))))

(setq x '(a b)
	listx (list x 1))

(eq x (car (copy-list listx)))  ;; t

(eq x (car (copy-tree listx))) ;; nil

(eq (car (copy-list listx)) (car (copy-tree listx)))

(car (copy-list listx))

(car (copy-tree listx))

(defun out-copy-tree (tree)
	(if (atom tree)
		tree
		(cons (our-copy-tree (car tree))
			(if (cdr tree) (our-copy-tree (cdr tree))))))

(defun count-leaves (tree)
	(if (atom tree)
		1
		(+ (count-leaves (car tree))
			(or (if (cdr tree) (count-leaves (cdr tree))) 1))))

(count-leaves '(a b (c d) (e)))

(count-leaves '((a b (c d)) (e) f))

'((a . (b . ((c . (d . nil)) . nil))) . ((e . nil) . (f . nil)))

(count-leaves '(a b))

(defun rfind-if (fn tree)
	(if (atom tree)
		(and (funcall fn tree) tree)
		(or (rfind-if fn (car tree))
			(if (cdr tree) (rfind-if fn (cdr tree))))))

(rfind-if (fint #'numberp #'oddp) '(2 (3 4) 5))

#|
	Chapter 6.
	Functions as Representation.
|#

(defstruct node contents yes no)

(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
	(setf (gethash name *nodes*)
		(make-node :contents conts
			:yes yes
			:no no)))

(defnode 'people "Is the person a man?" 'male 'female)

(defnode 'male "Is he living?" 'liveman 'deadman)

(defnode 'deadman "Was he American?" 'us 'them)

(defnode 'us "Is he on a coin?" 'coin 'cidence)

(defnode 'coin "Is the coin penny?" 'penny 'coins)

(defnode 'penny 'lincoln)

(defun run-node (name)
	(let ((n (gethash name *nodes*)))
		(cond ((node-yes n)
				  (format t "~A~%>> " (node-contents n))
				  (case (read)
					  (yes (run-node (node-yes n)))
					  (t (run-node (node-no n)))))
			(t (node-contents n)))))

(run-node 'people)

(setq n (compile-net 'people))

#|
	Chapter 7.
	Macros.
|#

(defmacro nil! (var)
	(list 'setq var nil))

(nil! x)

(defmacro nil! (var)
	`(setq ,var nil))

;; use backquote
(defmacro nif (expr pos zero neg)
	`(case (truncate (signum ,expr))
		 (1 ,pos)
		 (0 ,zero)
		 (-1 ,neg)))

;; use non-backquote
(defmacro nif (expr pos zero neg)
	(list 'case
		(list 'truncate (list 'signum expr))
		(list 1 pos)
		(list 0 zero)
		(list -1 neg)))

(mapcar #'(lambda (x)
			  (nif x 'p 'z 'n))
	'(0 2.5 -8))

(case (truncate (signum 2))
	(1 'p)
	(0 'z)
	(-1 'n))

(setq b '(1 2 3))

`(a ,b c) ;; (A (1 2 3) C)

`(a ,@b c) ;; (A 1 2 3 C)

`(a ,@1)

(defun greet (name)
	`(hello ,name))

(greet "tabuyos")

;; macro expand for nil!
(macroexpand-1 '(nil! x))

(member x choices :test #'eq)

(defmacro memq (obj lst)
	`(member ,obj ,lst :test #'eq))

(defmacro my-memq (obj lst)
	(list 'member obj lst ':test '#'eq))

(macroexpand-1 '(my-memq x choices))

(defmacro my-while (test &body body)
	`(do ()
		 ((not ,test))
		 ,@body))

(pprint (macroexpand '(my-while (able) (laugh))))

(defmacro mac (expr)
	`(pprint (macroexpand-1 ',expr)))

(mac (or x y))

(mac (nil! x))

(multiple-value-bind (exp bool) (macroexpand-1 '(memq 'a '(a b c))) (setq gexp exp))

gexp

(eval gexp)

;; destructuring
(defun foo (x y z)
	(+ x y z))

(foo 1 2 3)

(destructuring-bind (x (y) . z) '(a (b) c d)
	(list x y z))

(mac (dolist (x '(a b c)) (print x)))

(dolist (x '(a b c)) (print x))

(defmacro our-dolist ((var list &optional result) &body body)
	`(progn
		 (mapc #'(lambda (,var) ,@body) ,list)
		 (let ((,var nil)) ,result)))

(mac (our-dolist (x '(a b c)) (print x)))

(mapc #'(lambda (x) (print x)) '(a b c))

(let ((x nil)) t)

(defmacro when-bind ((var expr) &body body)
	`(let ((,var ,expr))
		 (when ,var ,body)))

(when-bind (input (get-user-input))
	(process input))

(defmacro our-expander (name) `(get ,name 'expander))

(mac (our-expander name))

;; gensym
(defmacro our-defmacro (name parms &body body)
	(let ((g (gensym)))
		`(progn
			 (setf (our-expander ',name)
				 #'(lambda (,g)
					   (block ,name
						   (destructuring-bind ,parms (cdr ,g)
							   ,@body))))
			 ',name)))

(defun our-macroexpand-1 (expr)
	(if (and (consp expr) (our-expander (car expr)))
		(funcall (our-expander (car expr)) expr)
		expr))

(let ((op 'setq))
	(defmacro our-setq (var val)
		(list op var val)))

(do ((w 3)
		(x 1 (1+ x))
		(y 2 (1+ y))
		(z))
	((> x 10) (princ z) y)
	(princ x)
	(princ y))

(prog ((w 3) (x 1) (y 2) (z nil))
	foo
	(if (> x 10)
		(return (progn (princ z) y)))
	(princ x)
	(princ y)
	(psetq x (1+ x) y (1+ y))
	(go foo))

;; (2 2)
(let ((a 1))
	(setq a 2 b a)
	(list a b))

;; psetq: parallel setq
;; (2 1)
(let ((a 1))
	(psetq a 2 b a)
	(list a b))

;; if we use setq instead of psetq, so do -> do*

(defmacro our-do (bindforms (test &rest result) &body body)
	(let ((label (gensym)))
		`(prog ,(make-initforms bindforms)
			 ,label
			 (if ,test
				 (return (progn ,@result)))
			 ,@body
			 (psetq ,@(make-stepforms bindforms))
			 (go ,label))))

(defun make-initforms (bindforms)
	(mapcar #'(lambda (b)
				  (if (consp b)
					  (list (car b) (cadr b))
					  (list b nil)))
		bindforms))

(defun make-stepforms (bindforms)
	(mapcan #'(lambda (b)
				  (if (and (consp b) (third b))
					  (list (car b) (third b))
					  nil))
		bindforms))

(defmacro our-and (&rest args)
	(case (length args)
		(0 t)
		(1 (car args))
		(t `(if ,(car args)
				(our-and ,@(cdr args))))))

(defmacro our-andb (&rest args)
	(if (null args)
		t
		(labels ((expander (rest)
					 (if (cdr rest)
						 `(if ,(car rest)
							  ,(expander (cdr rest)))
						 (car rest))))
			(expander args))))

(mac (our-andb '(a b c)))

(let ((args '(a b c d)))
    (print (labels ((expander (rest)
				  (if (cdr rest)
					  `(if ,(car rest)
						   ,(expander (cdr rest)))
					  (car rest))))
		 (expander args))))

(defmacro our-test-and(&rest args)
	(print args)
	(labels ((expander (rest)
				 (if (cdr rest)
					 `(if ,(car rest)
						  ,(expander (cdr rest)))
					 (car rest))))
		(expander args)))

(our-test-and 'a 'b 'c 'd)

;; (IF A
;;     (IF B
;;         (IF C
;;             D)))

(cdr '('(a b c)))

;; 0
(defmacro mac (x) `(1+ ,x))

;; 1
(setq fn (compile nil '(lambda (y) (mac y))))

;; 2
(defmacro mac (x) `(+ 1 ,x 100))

;; 01: 2, 21: 102
(funcall fn 1)

;; so first, we must be define before call macro
;; seconde, redefine a macro, we must be rebuild all method of called for the macro.

(defun secound (x) (cadr x))

(defmacro secound (x) `(cadr ,x))

(defun noisy-secound (x)
	(princ "Someone is taking a cadr!")
	(cadr x))

(defmacro noisy-second (x)
	`(progn
		 (princ "Someone is taking a cadr!")
		 (cadr ,x)))

(defun sum (&rest args)
	(apply #'+ args))

(defmacro sum (&rest args)
	`(apply #'+ (list ,@args)))

(defmacro sum (&rest args)
	`(+ ,@args))

(defun foo (x y z)
	(list x (let ((x y))
				(list x z))))

(defmacro foo (x y z)
	`(list ,x (let ((x ,y))
				  (list x ,z))))

(symbol-macrolet ((hi (progn (print "Howdy") 1)))
	(+ hi 2))

#|
	Chapter 8.
	When to use macros.
|#

(defun 1+ (x) (+ 1 x))

(defmacro while (test &body body)
	`(do ()
		 ((not ,test))
		 ,@body))

;; (do ()
;; 	((not <condition>))
;; 	<body of code>)

(defun move-objs (objs dx dy)
	(multiple-value-bind (x0 y0 x1 y1) (bounds objs)
		(dolist (o objs)
			(incf (obj-x o) dx)
			(incf (obj-y o) dy))
		(multiple-value-bind (xa ya xb yb) (bounds objs)
			(redraw (min x0 xa) (min y0 ya)
				(max x1 xb) (max y1 yb)))))

(defun scale-objs (objs factor)
	(multiple-value-bind (x0 y0 x1 y1) (bounds objs)
		(dolist (o objs)
			(setf (obj-dx o) (* (obj-dx o) factor)
				(obj-dy o) (* (obj-dy o) factor)))
		(multiple-value-bind (xa ya xb yb) (bounds objs)
			(redraw (min x0 xa) (min y0 ya)
				(max x1 xb) (max y1 yb)))))

(defmacro with-redraw ((var objs) &body body)
	(let ((gob (gensym))
			 (x0 (gensym)) (y0 (gensym))
			 (x1 (gensym)) (y1 (gensym)))
		`(let ((,gob ,objs))
			 (multiple-value-bind (,x0 ,y0 ,x1 ,y1) (bounds ,gob)
				 (dolist (,var ,gob) ,@body)
				 (multiple-value-bind (xa ya xb yb) (bounds ,gob)
					 (redraw (min ,x0 ,xa) (min ,y0 ya)
						 (max ,x1 xb) (max ,y1 yb)))))))

(defun move-objs (objs dx dy)
	(with-redraw (o objs)
		(incf (obj-x o) dx)
		(incf (obj-y o) dy)))

(defun scale-objs (objs factor)
	(with-redraw (o objs)
		(setf (obj-dx o) (* (obj-dx o) factor)
			(obj-dy o) (* (obj-dy o) factor))))

#|
	Chapter 9.
	Variable capture.
|#

(defmacro for ((var start stop) &body body)
	`(do ((,var ,start (1+ ,var))
			 (limit ,stop))
		 ((> ,var limit))
		 ,@body))

(for (x 1 5)
	(princ x))

(for (limit 1 5)
	(princ x))

(mac (for (limit 1 5) (print x)))

(let ((limit 5))
	(for (i 1 10)
		(when (> i limit)
			(princ i))))

(defvar w nil)

(defmacro gripe (warning)
	`(progn (setq w (nconc w (list ,warning)))
		 nil))

(defun sample-ratio (v w)
	(let ((vn (length v)) (wn (length w)))
		(if (or (< vn 2) (< wn 2))
			(gripe "sample < 2")
			(/ vn wn))))

(let ((lst '(b)))
	(sample-ratio nil lst)
	lst)

w

(defmacro foo (x y)
	`(/ (+ ,x 1) ,y))

(foo (- 5 2) 6)

(mac (foo (- 5 2) 6))

(defmacro cap1 ()
	'(+ x 1))

(defvar x 1)

;; 2
(mac (cap1))

(defmacro cap2 (var)
	`(let ((x ...)
			  (,var ...))
		 ...))

(defmacro cap3 (var)
	`(let ((x ...))
		 (let ((,var ...))
			 ...)))

(defmacro cap4 (var)
	`(let ((,var ...))
		 (let ((x ...))
			 ...)))

(defmacro safe1 (var)
	`(progn (let ((x 1))
				(print x))
		 (let ((,var 1))
			 (print ,var))))

(defmacro cap5 (&body body)
	`(let ((x ...))
		 ,@body))

(defmacro safe2 (expr)
	`(let ((x ,expr))
		 (cons x 1)))

(defmacro safe3 (var &body body)
	`(let ((,var ...))
		 ,@body))

(defmacro for ((var start stop) &body body)
	`(do ((,var ,start (1+ ,var))
			 (limit ,stop))
		 ((> ,var limit))
		 ,@body))

(let ((limit 0))
	(for (x 1 10)
		(incf limit x))
	limit)

(mac (for (x 1 10) (incf limit x)))

;; never stopping
(for (x 1 10) (incf limit x))

(let (( x 1)) (list x))

;; wrong
(defmacro pathological (&body body)
	(let* ((syms (remove-if (complement #'symbolp)
					 (flatten body)))
			  (var (nth (random (length syms))
					   syms)))
		`(let ((,var 99))
			 ,@body)))

;; avoid capture with better names
;; avoid capture by prior evaluation

;; easy capture ex:
(defmacro before (x y seq)
	`(let ((seq ,seq))
		 (< (position ,x seq)
			 (position ,y seq))))

;; simulate an capture example by use progn
(before (progn (setq seq '(b a)) 'a) 'b '(a b)) ;; nil

(mac (before (progn (setq seq '(b a)) 'a) 'b '(a b)))

;; right ex:
(defmacro before (x y seq)
	`(let ((xval ,x) (yval ,y) (seq ,seq))
		 (< (position xval seq)
			 (position yval seq))))

(before (progn (setq seq '(b a)) 'a) 'b '(a b)) ;; t

(mac (before (progn (setq seq '(b a)) 'a) 'b '(a b)))

;; vulnerable to capture
(defmacro for ((var start stop) &body body)
	`(do ((,var ,start (1+ ,var))
			 (limit ,stop))
		 ((> ,var limit))
		 ,@body))

;; a correct version
(defmacro for ((var start stop) &body body)
	`(do ((b #'(lambda (,var) ,@body))
			 (count ,start (1+ count))
			 (limit ,stop))
		 ((> count limit))
		 (funcall b count)))

;; wrong version
(defmacro for ((var start stop) &body body)
	`(do ((,var ,start (1+ ,var))
			 (xsf2jsh ,stop))
		 ((> ,var xsf2jsh))
		 ,@body))

(gensym)

*gensym-counter*

(setq x (gensym))

(setq *gensym-counter* 549 y (gensym))

(eq x y)

x

y

(format t "~A ~A" x y)

;; vulnerable to capture
(defmacro for ((var start stop) &body body)
	`(do ((,var ,start (1+ ,var))
			 (limit ,stop))
		 ((> ,var limit))
		 ,@body))

;; a correct version
(defmacro for ((var start stop) &body body)
	(let ((gstop (gensym)))
		`(do ((,var ,start (1+ ,var))
				 (,gstop ,stop))
			 ((> ,var ,gstop))
			 ,@body)))

#|
	Chapter 10.
	Other Macro Pitfalls.
|#

;; a correct version
(defmacro for ((var start stop) &body body)
	(let ((gstop (gensym)))
		`(do ((,var ,start (1+ ,var))
				 (,gstop ,stop))
			 ((> ,var ,gstop))
			 ,@body)))

;; subject to multiple evaluations
(defmacro for ((var start stop) &body body)
	`(do ((,var ,start (1+ ,var)))
		 ((> ,var ,stop))
		 ,@body))

;; incorrect order of evaluation
(defmacro for ((var start stop) &body body)
	(let ((gstop (gensym)))
		`(do ((,gstop ,stop)
				 (,var ,start (1+ ,var)))
			 ((> ,var ,gstop))
			 ,@body)))

(let ((x 2))
	(for (i 1 (incf x))
		(princ i)))

(setq x 10)

(+ (setq x 3) x)

(* 2 3)

(let ((x 1))
	(for (i x (setq x 13))
		(princ i)))

(defmacro nil! (x)
	(incf *nil!s*)
	`(setf ,x nill))

;; wrong
(defmacro string-call (opstring &rest args)
	`(,(intern opstring) ,@args))

(defun our+ (x y) (+ x y))

(string-call "OUR+" 2 3)

;; The call to intern takes a string and returns the correspondingsymbol

(defun et-al (&rest args)
	(nconc args (list 'et 'al)))

(et-al 'smith 'jones)

(setq greats '(leonardo michelangelo))

(apply #'et-al greats)

greats

(defmacro echo (&rest args)
	`',(nconc args (list 'amen)))

(defun foo () (echo x))

(foo) ;; x amen

(foo) ;; x amen

(defmacro echo (&rest args)
	`'(,@args amen))

(foo)

(defmacro crazy (expr) (nconc expr (list t)))

(defun foo () (crazy (list)))

(foo)

(defun our-length (x)
	(if (null x)
		0
		(1+ (our-length (cdr x)))))

(our-length '(1 2 3 4))

(defun our-length (x)
	(do ((len 0 (1+ len))
			(y x (cdr y)))
		((null y) len)))

(our-length '(1 2 3 4))

#|
	Chapter 11.
	Classic Macros.
|#

;; use lambda to implementation let
(defmacro our-let (binds &body body)
	`((lambda ,(mapcar #'(lambda (x)
							 (if (consp x) (car x) x))
				   binds)
		  ,@body)
		 ,@(mapcar #'(lambda (x)
						 (if (consp x) (cadr x) nil))
			   binds)))

(mac (our-let ((x 1) (y 2))
		 (+ x y)))

(defmacro when-bind ((var expr) &body body)
	`(let ((,var ,expr))
		 (when ,var
			 ,@body)))

(defmacro when-bind* (binds &body body)
	(if (null binds)
		`(progn ,@body)
		`(let (,(car binds))
			 (if ,(caar binds)
				 (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
	`(let ,(mapcar #'(lambda (s)
						 `(,s (gensym)))
			   syms)
		 ,@body))

(when-bind* ((x (find-if #'consp '(a (1 2) b)))
				(y (find-if #'oddp x)))
	(+ y 10))

;; ex
(defmacro with-redraw ((var objs) *body body)
	(let ((gob (gensym))
			 (x0 (gensym)) (y0 (gensym))
			 (x1 (gensym)) (y1 (gensym)))
		...))

(let ((sun-place 'park) (rain0place 'library))
	(if (sunny)
		(visit sun-place)
		(visit rain-place)))

(defmacro condlet (clauses &body body)
	(let ((bodfn (gensym))
			 (vars (mapcar #'(lambda (v) (cons v (gensym)))
					   (remove-duplicates
						   (mapcar #'car (mappend #'cdr clauses))))))
		`(labels ((,bodfn ,(mapcar #'car vars)
					  ,@body))
			 (cond ,@(mapcar #'(lambda (cl)
								   (condlet-clause vars cl bodfn))
						 clauses)))))

(defun condlet-clause (vars cl bodfn)
	`(,(car cl) (let ,(mapcar #'cdr vars)
					(let ,(condlet-binds vars cl)
						(,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
	(mapcar #'(lambda (bindform)
				  (if (consp bindform)
					  (cons (cdr (assoc (car bindform) vars))
						  (cdr bindform))))
		(cdr cl)))

(defun mappend (fn &rest lsts)
	(apply #'append (apply #'mapcar fn lsts)))

(condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
			 ((= 1 1) (y (princ 'c)) (x (princ 'd)))
			 (t (x (princ 'e)) (z (princ 'f))))
	(list x y z))

(apply #'append '((apply #'mapcar #'1+ '((1 2 3)))))

(with-open-file (s "dump" :direction :output)
	(princ 99 s))

(mac (with-open-file (s "dump" :direction :output)
		 (princ 99 s)))

(setq x 'a)

(unwind-protect
	(progn (princ "What error?")
		(error "This error."))
	(setq x 'b))

x

(let ((temp *db*))
	(setq *db* db)
	(lock *db*)
	(prog1 (eval-query q)
		(release *db*)
		(setq *db* temp)))

(with-db db
	(eval-query q))

;; implementation to with-db by macro
(defmacro with-db (db &body body)
	(let ((temp (gensym)))
		`(let ((,temp *db*))
			 (unwind-protect
				 (progn
					 (setq *db* ,db)
					 (lock *db*)
					 ,@body)
				 (progn
					 (release *db*)
					 (setq *db* ,temp))))))

;; implementation to with-db by macro and function
(defmacro with-db (db &body body)
	(let ((gbod (gensym)))
		`(let ((,gbod #'(lambda () ,@body)))
			 (declare (dynamic-extent ,gbod))
			 (with-db-fn *db* ,db ,gbod))))

(defun with-db-fn (old-db new-db body)
	(unwind-protect
		(progn
			(setq *db* new-db)
			(lock *db*)
			(funcall body))
		(progn
			(release *db*)
			(setq *db* old-db))))

(if t
	'phew
	(/ 1 0))

(while (not sick)
	(if3 (cake-permitted)
		(eat-cake)
		(throw 'tantrum nil)
		(plead-insistently)))

(defmacro if3 (test t-case nil-case ?-case)
	`(case ,test
		 ((nil) ,nil-case)
		 (? ,?-case)
		 (t ,t-case)))

;; numeric if
(defmacro nif (expr pos zero neg)
	(let ((g (gensym)))
		`(let ((,g ,expr))
			 (cond ((plusp ,g) ,pos)
				 ((zerop ,g) ,zero)
				 (t ,neg)))))

(mapcar #'(lambda (x)
			  (nif x 'p 'z 'n))
	'(0 1 -1))

(let ((x (foo)))
	(or (eql x (bar)) (eql x (baz))))

(member (foo) (list (bar) (baz)))

;; the equivalent in expression
(in (foo) (bar) (baz))

(macroexpand '(in (foo) (bar) (baz)))

(inq operator + - *)

(in operator '+ '- '*)

(defmacro in (obj &rest choices)
	(let ((insym (gensym)))
		`(let ((,insym ,obj))
			 (or ,@(mapcar #'(lambda (c)) `(eql ,insym ,c))
				 choices))))

(defmacro inq (obj &rest args)
	`(in ,obj ,@(mapcar #'(lambda (a)
							  `',a)
					args)))

(defmacro in-if (fn &rest choices)
	(let ((fnsym (gensym)))
		`(let ((,fnsym ,fn))
			 (or ,@(mapcar #'(lambda (c)
								 `(funcall ,fnsym ,c))
					   choices)))))

(defmacro >case (expr &rest clauses)
	(let ((g (gensym)))
		`(let ((,g ,expr))
			 (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
						 clauses)))))

(defmacro >casex (g cl)
	(let ((key (car cl)) (rest (cdr cl)))
		(cond ((consp key) `((in ,g ,@key) ,@rest))
			((inq key t otherwise) `(t ,@rest))
			(t (error "bad >case clause")))))

(member x (list 'a 'b) :test #'equal)

(setq a 1 b 2)

(in-if #'(lambda (y) (equal x y)) 'a 'b)

(some #'oddp (list a b))

(in-if #'oddp a b)

(mac (case a ('a 1)))

(defmacro forever (&body body)
	`(do ()
		 (nil)
		 ,@body))

(defmacro while (test &body body)
	`(do ()
		 ((not ,test))
		 ,@body))

(defmacro till (test &body body)
	`(do ()
		 (,test)
		 ,@body))

(defmacro for ((var start stop) &body body)
	(let ((gstop (gensym)))
		`(do ((,var ,start (1+ ,var))
				 (,gstop ,stop))
			 ((> ,var ,gstop))
			 ,@body)))

(setq y 12 x 21)

y

x

(do-tuples/o (x y) `(a b c d)
	(princ (list x y)))

(defmacro do-tuples/o (parms source &body body)
	(if parms
		(let ((src (gensym)))
			`(prog ((,src ,source))
				 (mapc #'(lambda ,parms ,@boyd)
					 ,@(map0-n #'(lambda (n)
									 `(nthcdr ,n ,src))
						   (- (length source)
							   (length parms))))))))

(defmacro to-tuples/c (parms source &body body)
	(if parms
		(with-gensyms (src rest bodfn)
			(let ((len (length parms)))
				`(let ((,src ,source))
					 (when (nthcdr ,(1- len) ,src)
						 (labels ((,bodfn ,parms ,@body))
							 (do ((,rest ,src (cdr ,rest)))
								 ((not (nthcdr ,(1- len) ,rest))
									 ,@(mapcar #'(lambda (args)
													 `(,bodfn @args))
										   (dt-args len rest src))
									 nil)
								 (,bodfn @(map1-n #'(lambda (n)
														`(nth ,(1- n)
															 ,rest))
											  len))))))))))

(defun dt-args (len rest src)
	(map0-n #'(lambda (m)
				  (map1-n #'(lambda (n)
								(let ((x (+ m n)))
									(if (>= x len)
										`(nth ,(- x len) ,src)
										`(nth ,(1- x) ,rest))))
					  len))
		(- len 2)))

(do-tuples/c (x y) '(a b c d)
	(princ (list x y)))

(mvdo* ((x 1 (1+ x))
		   ((y z) (values 0 0) (values z x)))
	((> x 5) (list x y z))
	(princ (list x y z)))

(defmacro mvdo* (parm-cl test-cl &body body)
	(mvdo-gen parm-cl parm-cl test-cl body))

(defun mvdo-gen (binds rebinds test body)
	(if (null binds)
		(let ((label (gensym)))
			`(prog nil
				 ,label
				 (if ,(car test)
					 (return (progn ,@(cdr test))))
				 ,@body
				 ,@(mvdo-rebind-gen rebinds)
				 (go ,label)))
		(let ((rec (mvdo-gen (cdr binds) rebinds test body)))
			(let ((var/s (caar binds)) (expr (cadar binds)))
				(if (atom var/s)
					`(let ((,var/s ,expr)) ,rec)
					`(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
	(cond ((null rebinds) nil)
		((< (length (car rebinds)) 3)
			(mvdo-rebind-gen (cdr rebinds)))
		(t
			(cons (list (if (atom (caar rebinds))
							'setq
							'multiple-value-setq)
					  (caar rebinds)
					  (third (car rebinds)))
				(mvdo-rebind-gen (cdr rebinds))))))

(mvdo ((x 1 (1+ x))
		  ((y z) (values 0 0) (values z x)))
	((> x 5) (list x y z))
	(princ (list x y z)))

(let ((w 0) (x 1) (y 2) (z 3))
	(mvpsetq (w x) (values 'a 'b) (y z) (values w x))
	(list w x y z))

(shuffle '(a b c) '(1 2 3 4))

(defun fnif (test then &optional else)
	(if test
		(funcall then)
		(if else (funcall else))))

(if (rich) (go-sailing) (rob-bank))

;; equal
(fnif (righ)
	#'(lambda () (go-sailing))
	#'(lambda () (rob-bank)))

(defmacro mvpsetq (&rest args)
	(let* ((pairs (group args 2))
			  (syms (mapcar #'(lambda (p)
								  (mapcar #'(lambda (x) (gensym))
									  (mklist (car p))))
						pairs)))
		(labels ((rec (ps ss)
					 (if (null ps)
						 `(setq
							  ,@(mapcan #'(lambda (p s)
											  (shuffle (mklist (car p))
												  s))
									pairs syms))
						 (let ((body (rec (cdr ps) (cdr ss))))
							 (let ((var/s (caar ps))
									  (expr (cadar ps)))
								 (if (consp var/s)
									 `(multiple-value-bind ,(car ss) ,expr ,body)
									 `(let ((,@(car ss) ,expr))
										  ,body)))))))
			(rec pairs syms))))

(defun shuffle (x y)
	(cond ((null x) y)
		((null y) x)
		(t (list* (car x) (car y)
			   (shuffle (cdr x) (cdr y))))))

(dolist (b bananas)
	(peel b)
	(eat b))

(mapc #'(lambda (b)
			(peel b)
			(eat b))
	bananas)

(defun forever (fn)
	(do ()
		(nil)
		(funcall fn)))

(defmacro mvdo (binds (test &rest result) &body body)
	(let ((label (gensym))
			 (temps (mapcar #'(lambda (b)
								  (if (listp (car b))
									  (mapcar #'(lambda (x)
													(gensym))
										  (car b))
									  (gensym)))
						binds)))
			 `(let ,(mappend #'mklist temps)
				  (mvpsetq ,@(mapcan #'(lambda (b var)
										   (list var (cadr b)))
								 binds temps))
				  (prog ,(mapcar #'(lambda (b var) (list b var))
							 (mappend #'mklist (mapcar #'car binds))
							 (mappend #'mklist temps))
					  ,label
					  (if ,test
						  (return (progn ,@result)))
					  ,@body
					  (mvpsetq ,@(mapcan #'(lambda (b)
											   (if (third b)
												   (list (car b)
													   (third b))))
									 binds))
					  (go ,label)))))

(mvdo ((x 1 (1+ x))
		  ((y z) (values 0 0) (values z x)))
	((> x 5) (list x y z))
	(princ (list x y z)))

#|
	Chapter 12.
	Generalizes Variables.
|#

(setf lst '(a b c))

(setf (car lst) 480)

lst

(progn (rplaca lst 480) 480)

(defmacro toggle (obj)
	`(setf ,obj (not ,obj)))

(let ((lst '(a b c)))
	(toggle (car lst)) lst)

(defvar *friends* (make-hash-table))

(setf (gethash 'mary *friends*) (make-hash-table))

(setf (gethash 'john (gethash 'mary *friends*)) t)

(gethash 'mary *friends*)

(setf (gethash x (gethash y *friends*))
	(not (gethash x (gethash y *friends*))))

(defmacro friend-of (p q)
	`(gethash ,p (gethash ,q *friends*)))

(toggle (friend-of x y))

;; wrong
(defmacro toggle (obj)
	`(setf ,obj (not ,obj)))

(setq i 2)

(toggle (nth (incf i) lst))

(let ((lst '(t nil t))
		 (i -1))
	(toggle (nth (incf i) lst))
	lst)

(define-modify-macro toggle () not)

(let ((lst '(t nil t))
		 (i -1))
	(toggle (nth (incf i) lst))
	lst)

(defmacro allf (val &rest args)
	(with-gensyms (gval)
		`(let ((,gval ,val))
			 (setf ,@(mapcan #'(lambda (a) (list a gval))
						 args)))))

(defmacro nilf (&rest args) `(allf nil ,@args))

(defmacro tf (&rest args) `(allf t ,@args))

(defmacro toggle (&rest args)
	`(progn
		 ,@(mapcar #'(lambda (a) `(toggle2 ,a))
			   args)))

(define-modify-macro toggle2 () not)

(setf x 1 y 2)

(setf x nil y nil z nil)

(nilf x y z)

(define-modify-macro concf (obj) nconc)

(defun conc1f/function (place obj)
	(conc place (list obj)))

(define-modify-macro conc1f (obj) con1f/fucntion)

(defun concnew/function (place obj &rest args)
	(unless (apply #'member obj place args)
		(conc place (list obj))))

(define-modify-macro concnew (obj &rest args)
	concnew/function)

(setq x (nconc x y))

(define-modify-macro conc1f (obj) conc1)

;; appen element in tail to construct list, you should use push and nreverse in finally
;; handle data in head than better in foot tail

(incf x y)

x

(setf (obj-dx o) (* (obj-dx o) factor))

(_f * (obj-dx o) factor)

;; wrong
(defmacro _f (op place &rest args)
	`(setf ,place (,op ,place ,@args)))

(incf (aref a (incf i)))

(get-setf-expansion '(aref a (incf i)))

(let* ((#:g4 af
		   #:g5 (incf i))))

;; a corret version
(defmacro _f (op place &rest args)
	(multiple-value-bind (vars forms var set access)
		(get-setf-expansion place)
		`(let* (,@(mapcar #'list vars forms)
				   (,(car var) (,op ,access ,@args)))
			 ,set)))

(defmethod pull (obj place &rest args)
	(multiple-value-bind (vars forms var set access)
		(get-setf-expansion place)
		(let ((g (gensym)))
			`(let* ((,g ,obj)
						,@(mapcar #'list vars forms)
						(,(car var) (delte ,g ,access ,@args)))
					   ,set))))
(defmacro pull-if (test place &rest args)
	(multiple-value-bind (vars forms var set access)
		(get-setf-expansion place)
		(let ((g (gensym)))
			`(let* ((,g ,test)
				,@(mapcar #'list vars forms)
				(,(car var) (delete-if ,g ,access ,@args)))
				 ,set))))

(defmacro popn (n place)
	(multiple-value-bind (vars forms var set access)
		(get-setf-expansion place)
		(with-gensyms (gen glst)
			`(let* ((,gn nf
						,@(mapcar #'list vars forms)
						(,glst ,access)
						(,(car var) (nthcdr ,gn ,glst)))
					   (prog1 (subseq ,glst 0 ,gn)
						   ,set))))))

(_f memoize (symbol-function 'foo))

(defmacro conc1f (lst obj)
	`(_f nconc ,lst (list ,obj)))

(setq x '(1 2 (a b) 3))

(pull 2 x)

(pull '(a b) x :test #'equal)

(defmacro pull (obj seq &rest args)
	`(setf ,seq (delete ,obj ,seq ,@args)))

(define-modify-macro pull (obj &rest args)
	(lambda (seq obj &rest args)
		(apply #'delete obj seq args)))

(let ((lst '(1 2 3 4 5 6)))
	(pull-if #'oddp lst)
	lst)

(setq x '(a b c d e f))

(popn 3 x)

(defmacro sortf (op &rest places)
	(let* ((meths (mapcar #'(lambda (p)
								(multiple-value-list
									(get-setf-expansion p)))
					  places))
			  (temps (apply #'append (mapcar #'third meths))))
		`(let* ,(mapcar #'list
					(mapcan #'(lambda (m)
								  (append (first m)
									  (third m)))
						meths)
					(mapcan #'(lambda (m)
								  (append (second m)
								  (list (fifth m))))
					meths))
		,@(mapcon #'(lambda (rest)
						(mapcar
							#'(lambda (arg)
								  `(unless (,op ,(car rest) ,arg)
									   (rotated ,(car rest) ,arg)))
							(cdr rest)))
			  temps)
			 ,@(mapcar #'fourth meths))))

(setq x 1 y 2 z 3)

(sortf > x y z)

(defsetf symbol-value set)

(defsetf our-car (lst) (new-car)
	`(progn (rplaca ,lst ,new-car)
		 ,new-car))

(defun (setf car) (new-car lst)
	(rplaca lst new-car)
	new-car)

(defvar *cache* (make-hash-table))

(defun retrieve (key)
	(multiple-value-bind (x y) (gethash key *cache*)
		(if y
			(values x y)
			(cdr (assoc key *world*)))))

(defsetf retrieve (eky) (val)
	`(setf (gethash ,key *cache*) ,val))

#|
	Chapter 13.
	Computation at Compile-Time.
|#

(defun avg (&rest args)
	(/ (apply #'+ args) (length args)))

(defmacro avg (&rest args)
	`(/ (+ ,@args) ,(length args)))

(defun most-of (&rest args)
	(let ((all 0)
			 (hits 0))
		(dolist (a args)
			(incf all)
			(if a (incf hits)))
		(> hits (/ all 2))))

(defmacro most-of (&rest args)
	(let ((need (floor (/ (length args) 2)))
			 (hits (gensym)))
		`(let ((,hits 0))
			 (or ,@(mcapcar #'(lambda (a)
								  `(and ,a (> (incf ,hits) ,need)))
					   args)))))

(defun nthmost (n lst)
	(nth n (sort (copy-list lst) #'>)))

(defmacro nthmost (n lst)
	(if (and (integerp n) (< n 20))
		(with-gensyms (glst gi)
		(let ((syms (map0-n #'(lambda (x) (gensym)) n)))
			`(let ((,glst ,lst))
				 (unless (< (length ,glst) ,(1+ n))
					 ,@(gen-start glst syms)
					 (dolist (,gi ,glst)
						 ,(nthmost-gen gi syms t))
					 ,(car (last syms))))))
		`(nth ,n (sort (copy-list ,lst) #'>))))

(defun gen-start (glst syms)
	(reverse
		(maplist #'(lambda (syms)
					   (let ((var (gensym)))
						   `(let ((,var (pop ,glst)))
								,(nthmost-gen var (reverse syms)))))
			(reverse syms))))

(defun nthmost-gen (var vars &optional long?)
	(if (null vars)
		nil
		(let ((else (nthmost-gen var (cdr vars) long?)))
			(if (and (not long?) (null else))
				`(setq ,(car vars) ,var)
				`(if (> ,var ,(car vars))
					 (setq ,@(mapcan #'list
								 (reverse vars)
								 (cdr (reverse vars)))
						 ,(car vars) ,var)
					 ,else)))))

(nthmost 2 '(2 6 1 5 3 4))

#|
	Chapter 14.
	Anaphoric Macros.
|#

(let ((result (big-long-calculation)))
	(if result
		(foo result)))

(if (big-long-calculation)
	(foo it))

;; anaphor is expression of somethine in nature languages

(defmacro aif (test-form then-form &optional else-form)
	`(let ((if ,test-form))
		 (if it ,then-form ,else-form)))

(aif (big-long-calculation)
	(foo it))

(let ((it (big -long-calculation)))
	(if it (foo it) nil))

(find-if #'oddp '(2 4 6))				;; return nil

(find-if #'null '(2 nil 6))				;; we can't sure nil is return or findin

(setf edible (make-hash-table)
	(gethash 'olive-oil edible) t
	(gethash 'motor-oil edible) nil)

(gethash 'motor-oil edible)

(defun edible? (x)
	(multiple-value-bind (val found?) (gethash x edible)
		(if found?
			(if val 'yes 'no)
			'maybe)))

(mapcar #'edible? '(motor-oil olive-oil iguana))

#|
	Chapter 15.
	Macros Returning Functions.
|#

(defmacro fn (expr) `#',(rbuild expr))

(defun rbuild (expr)
	(if (or (atom expr) (eq (car expr) 'lambda))
		expr
		(if (eq (car expr) ''compose)
			(build-compose (cdr expr))
			(build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
	(let((g (gensym)))
		`(lambda (,g)
			 (,op ,@(mapcar #'(lambda (f)
								  `(,(rbuild f) ,g))
						fns)))))

(defun build-compose (fns)
	(let ((g (gensym)))
		`(lambda (,g)
			 ,(labels ((rec (fns)
						   (if fns
							   `(,(rbuild (car fns))
									,(rec (cdr fns)))
							   g)))
				  (rec fns)))))

(defun our-every (fn lst)
	(if (nul lst)
		t
		(and (funcall fn (car lst))
			(out-every fn (cdr lst)))))

(lrec #'(lambda (x f) (and (oddp x) (funcall f)))
	t)

(alrec (and (oodp it) rec) t)

(funcall (alrec (and (oddp it) rec) t) '(1 3 5))

(defmacro atrec (rec &optioncal (base 'it))
	"cltl2 version"
	(let ((lfn (gensym)) (rfn (gensym)))
		`(trec #'(lambda (it ,lfn ,rfn)
					 (symbol-macrolet ((left (funcall ,lfn))
										  (right (funcall ,rfn)))
						 ,rec))
			 #'(lambda (it) ,base))))

(defmacro atrec (rec &optional (base 'it))
	"cltl1 version"
	(let ((lfn (gensym)) (rfn (gensym)))
			 `(trec #'(lambda (it ,lfn ,rfn)
						  (labels ((left () (funcall ,lfn))
									  (right () (funcall ,rfn)))
							  ,rec))
				  #'(lambda (it) ,base))))

(defmacro on-trees (rec base &rest trees)
	`(funcall (atrec ,rec ,base) ,@trees))

(defun our-copy-tree (tree)
	(on-trees (cons left right) it tree))

(defun count-leaves (tree)
	(ontrees (+ left (or right 1)) 1 tree))

(defun flatten (tree)
	(on-trees (nconc left right) (mklist it) tree))


(defun refind-if (fn tree)
	(ontrees (or left right)
		(and (funcall fn it) it)
		tree))

(let (( x 2))
	(setq d (delay (1+ x))))

(force 'a)

(defconstant unforced (gensym))

;; define a struct for delay, and it have two properties: forced closure
(defstruct delay forced closure)

(defmacro delay (expr)
	(let ((self (gensym)))
		`(let ((,self (make-delay :forced unforced)))
			 (setf (delay-closure ,self)
				 #'(lambda ()
					   (setf (delay-forced ,self) ,expr)))
			 ,self)))

(defun force (x)
	(if (delay-p x)
		(if (eq (delay-forced x) unforced)
			(funcall (delay-closure x))
			(delay-forced x))
		x))

(setq d 2)

(force d)

(sort lst #'(lambda (x y) (> (force x) (force y))))

#|
	Chapter 16.
	Macro-Defining Macros
|#

(let ((a (car x)) (b (cdr x))) ...)

(destructuring-bind (a . b) x ...)

;; so we can do it
(defmacro dbing (&rest args)
	`(destructuring-bind ,@args))

(defmacro mvbind (&rest args)
	`(multiple-value-bind ,@args))

(abbrev mvbind multiple-value-bind)

(defmacro abbrev (short long)
	`(defmacro ,short (&rest args)
		 `(,',long ,@args)))

(defmacro abbrevs (&rest names)
	`(progn
		 ,@(mapcar #'(lambda (pair)
						 `(abbrev ,@pair))
			   (group names 2))))

(defmacro mvbind (&rest args)
	`(multiple-value-bind ,@args))

(defmacro mvbind (&rest args)
	(let ((name 'multiple-value-bind))
		`(,name ,@args)))

`(defmacro ,short (&rest args)
	 (let ((name ',long))
		 `(,name ,@args)))

`(defmacro ,short (&rest args)
	 `(,',long ,@args))

(setf (get o p) v)

(setf (get 'ball1 'color) 'red)

(defmacro color (obj)
	`(get ,obj 'color))

(color 'ball1)

(setf (color 'ball1) 'green)

(defmacro weight (obj)
	`(get ,obj 'weight))

(defmacro propmacro (propname)
	`(defmacro ,propname (obj)
		 `(get ,obj ',',propname)))

(defmacro propmacros (&rest props)
	`(progn
		 ,@(mapcar #'(lambda (p) `(propmacro ,p)
						 props))))

(propmacro color)

(defmacro color (obj)
	`(get ,obj 'color))

(defmacro color (obj)
	(let ((p 'color))
		`(get ,obj ',p)))

`(defmacro ,propname (obj)
	 (let ((p ',propname))
		 `(get ,obj ',p)))

`(defmacro ,propname (obj)
	 `(get ,obj ',',propname))

(let ((res (complicated-query)))
	(if res
		(foo res)))

(aif (complicated-query)
	(foo it))

(let ((o (owner x)))
	(and o (let ((a (address o)))
			   (and a (city a)))))

(aand (owner x) (address it) (city it))

(defun mass-cost (menu-price)
	(a+ menu-price (* it .05) (* it 3)))

(mass-cost 7.95)

(defmacro a+ (&rest args)
	(a+expand args nil))

(defun a+expand (args syms)
	(if args
		(let ((sym (gensym)))
			`(let* ((,sym ,(car args))
					   (it ,sym))
				 ,(a+expand (cdr args)
					  (append syms (list sym)))))
		`(+ ,@syms)))

(defmacro alist (&rest args)
	(alist-expand args nil))

(defun alist-expand (args syms)
	(if args
		(let ((sym (gensym)))
			`(let* ((,sym ,(car args))
					   (it ,sym))
				 ,(alist-expand (cdr args)
					  (append syms (list sym)))))
		`(list ,@syms)))

(a+ menu-price (* it .05) (* it 3))

(alist 1 (+ 2 it) (+ 2 it))

(defmacro a+ (&rest args)
	(anaphex args '(+)))

(defmacro defanaph (name &optional calls)
	(let ((calls (or calls (pop-symbol name))))
		`(defmacro ,name (&rest args)
			 (anaphex args (list ',calls)))))

(defun anaphex (args expr)
	(if args
		(let ((sym (gensym)))
			`(let* ((,sym ,(car args))
					   (it ,sym))
				 ,(anaphex (cdr args)
					  (append expr (list sym)))))
		expr))

(defun pop-symbol (sym)
	(intern (subseq (symbol-name sym) 1)))

(defun anaphex2 (op args)
	`(let ((it ,(car args)))
		 (,op it ,@(cdr args))))

(defmacro aif (&rest args)
	(anaphex2 'if args))

(defun anaphex3 (op args)
	`(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

(defmacro asetf (&rest args)
	(anaphex3 '(lambda (x y) (declare (ignore x)) y) args))

(defmacro defanaph (name &key calls (rule :all))
	(let* ((opname (or calls (popsymbol name)))
			  (body (case rule
						(:all `(anaphex1 args '(,opbname)))
						(:first `(anaphex2 ',opname args))
						(:place `(anaphex3 ',opname args)))))
		`(defmacro ,name (&rest args)
			 ,body)))

(defun ananphex1 (args call)
	(if args
		(let ((sym (gensym)))
			`(let* ((,sym ,(car args))
					   (it ,sym))
				 ,(anaphex1 (cdr args)
					  (append call (list sym)))))
		call))

(defun ananphex2 (op args)
	`(let ((it ,(car args))) (,op it ,@(cdr args))))

(defun ananphex3 (op args)
	`(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

(defmacro incf (place &optional (val 1))
	`(asetf ,place (+ it ,val)))

(defmacro pull (obj place &rest args)
	`(asetf ,place (delete ,obj it ,@args)))

#|
	Chapter 17.
	Read-Macros.
|#

(set-macro-character #\'
	#'(lambda (stram char)
		  (declare (ignore char))
		  (list 'quote (read stream t nil t))))

;; ' is oldest read-macro in lisp, you can 'a -> (quote a), but this very bother and reduce code readability

(setq a 1)

(defmacro q (obj)
	`(quote ,obj))

(eq 'c (q c))

(q (q a)) ;; error

(mac (q (q a))) ;; '(Q A)

(set-dispatch-macro-character #\# #\?
	#'(lambda (stream char1 char2)
		  (declare (ignore char1 char2))
		  `#'(lambda (&rest ,(gensym))
				 ,(read stream t nil t))))

(mapcar #?2 '(a b c))

(eq (funcall #?'a) 'a)

(eq (funcall #?#'oddp) (symbol-function 'oddp))

(set-macro-character #\] (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[
	#'(lambda (stream char1 char2)
		  (declare (ignore char1 char2))
		  (let ((accum nil)
				   (pair (read-delimited-list #\] stream t)))
			  (do ((i (ceiling (car pair)) (1+ i)))
				  ((> i (floor (cadr pair)))
				   (list 'quote (nreverse accum)))
				  (push i accum)))))

(format t "~A" #[2 7])

(defmacro defdelim (left right parms &body body)
	`(ddfn ,left ,right #'(lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\))))
	(defun ddfn (left right fn)
		(set-macro-character right rpar)
		(set-dispatch-macro-character #\# left
			#'(lambda (stream char1 char2)
				  (declare (ignore char1 char2))
				  (apply fn (read-delimited-list right stream t))))))

(defdelim #\{ #\} (&rest args)
	`(fn (compose ,@args)))

(defmacro quotable ()
	'(list 'able))

(defmacro quotable ()
	(quote (list (quote able))))

#|
	Chapter 18.
	Destructuring.
|#

;; CLTL1
(let ((x (first lst))
		 (y (second lst))
		 (z (third lst)))
	...)

;; CLTL2
(destructuring-bind (x y z) lst
	...)

;; destructuring-bind's first argument is any tree.
;; ex.
(destructuring-bind ((first last) (month day year) . notes) birthday
	...)

(defmacro dbind (pat seq &body body)
	(let ((gseq (gensym)))
		`(let ((,gseq ,seq))
			 ,(dbind-ex (destruc pat gseq #'atom) body))))

(defun destruc (pat seq &optional (atom? #'atom) (n 0))
	(if (null pat)
		nil
		(let ((rest (cond ((funcall atom? pat) pat)
						((eq (car pat) '&rest) (cadr pat))
						((eq (car pat) '&body) (cadr pat))
						(t nil))))
			(if rest
				`((,rest (subseq ,seq ,n)))
				(let ((p (car pat))
						 (rec (destruc (cdr pat) seq atom? (1+ n))))
					(if (funcall atom? p)
						(cons `(,p (elt ,seq ,n))
							rec)
						(let ((var (gensym)))
							(cons (cons `(,var (elt ,seq ,n))
									  (destruc p var atom?))
								rec))))))))

(defun dbind-ex (binds body)
	(if (null binds)
		`(progn ,@body)
		`(let ,(mapcar #'(lambda (b)
							 (if (consp (car b))
								 (car b)
								 b))
				   binds)
			 ,(dbind-ex (mapcan #'(lambda (b)
									  (if (consp (car b))
										  (cdr b)))
							binds)
				  body))))

(dbind (a b c) #(1 2 3) (list a b c))

(dbind (a (b c) d) '(1 #(2 3) 4) (list a b c d))

(dbind (a (b . c) &rest d) '(1 "fribble" 2 3 4) (list a b c d))
;; #(: vector #\: char "abc" = #(#\a #\b #\c)

(destruc '(a b c) 'seq #'atom)

(destruc '(a (b . c) &rest d) 'seq)

(dbind-ex (destruc '(a (b . c) &rest d) 'seq) '(body))

(dbind (a b c) (list 1 2))

(setq ar (make-array '( 3 3)))

(for (r 0 2)
	(for (c 0 2)
		(setf (aref ar r c) (+ (* r 10) c))))

(with-matrix ((a b c)
				 (d e f)
				 (g h i)) ar
	(list a b c d e f g h i))

(defmacro with-matrix (pats ar &body body)
	(let ((gar (gensym)))
		`(let ((,gar ,ar))
			 (let ,(let ((row -1))
					   (mapcan
						   #'(lambda (par)
								 (incf row)
								 (let ((col -1))
									 (mapcar #'(lambda (p)
												   `(,p (aref ,gar ,row ,(incf col))))
										 par)))
						   pats))
				 ,@body))))

(defmacro with-array (pat ar &body body)
	(let ((gar (gensym)))
		`(let ((,gar ,ar))
			 (let ,(mapcar #'(lambda (p)
								 `(,(car p) (aref ,gar ,@(cdr p))))
					   pat)
				 ,@body))))

(with-array ((a 0 0) (d 1 1) (i 2 2)) ar (values a d i))

(defmacro with-struct ((name . fields) struct &body body)
	(let ((gs (gensym)))
		`(let ((,gs ,struct))
			 (let ,(mapcar #'(lambda (f)
								 `(,f (,(symb name f) ,gs)))
					   fields)
				 ,@body))))

(defstruct visitor name title firm)

(setq theo (make-visitor :name "Theodebert" :title 'king :firm 'franks))

(with-struct (visitor- name firm title) theo
	(list name firm title))

(with-slots (species age height) my-tree
	...)

(setq height 72)

(defmacro with-places (pat seq &body body)
	(let ((gseq (gensym)))
		`(let ((,gseq ,seq))
			 ,(wplac-ex (destruc pat gseq #'atom) body))))

(defun wplac-ex (binds body)
	(if (null binds)
		`(progn ,@body)
		`(symbol-macrolet ,(mapcar #'(lambda (b)
										 (if (consp (car b))
											 (car b)
											 b))
							   binds)
			 ,(wplac-ex (mapcan #'(lambda (b)
									  (if (consp (car b))
										  (cdr b)))
							binds)
				  body))))

(with-places (a b c) #(1 2 3)
	(list a b c))

;; must be use setf!
(let ((lst '(1 (2 3) 4)))
	(with-places (a (b . c) d) lst
		(setf a 'uno)
		(setf c '(tre)))
	lst)

;; dbind faster than with-places

(match '(p a b c a) '(p ?x ?y c ?x))

(match '(p ?x b ?y a) '(p /y b c a))

(match '(a b c) '(a a a))

(match '(p ?x) '(p ?x))

(defun match (x y &optional binds)
	(acond2
		((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
		((binding x binds) (match it y binds))
		((binding y binds) (match x it binds))
		((varsym? x) (values (cons (cons x y) binds) t))
		((varsym? y) (values (cons (cons y x) binds) t))
		((and (consp x) (consp y) (match (car x) (car y) binds))
			(match (cdr x) (cdr y) it))
		(t (values nil nil))))

(defun varsym? (x)
	(and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
	(labels ((recbind (x binds)
				 (aif (assoc x binds)
					 (or (recbind (cdr it) binds)
						 it))))
		(let ((b (recbind x binds)))
			(values (cdr b) b))))

(match '(a ?x b) '(_ 1 _))

(defmacro if-match (pat seq then &optional else)
	`(aif2 (match ',pat ,seq)
		 (let ,(mapcar #'(lambda (v)
							 `(,v (binding ',v it)))
				   (vars-in then #'atom))
			 ,then)
		 ,else))

(defun vars-in (expr &optional (atom? #'atom))
	(if (funcall atom? expr)
		(if (var? expr) (list expr))
		(union (vars-in (car expr) atom?)
			(vars-in (cdr expr) atom?))))

(defun var? (x)
	(and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun abab (seq) (if-match (?x ?y /x ?y) seq (values ?x ?y) nil))

(abab '(hi ho hi ho))

(defmacro if-match (pat seq then &optional else)
	`(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
			   (vars-in pat #'simple?))
		 (pat-match ,pat ,seq ,then ,else)))

(defmacro pat-match (pat seq then else)
	(if (simple? pat)
		(match1 `((,pat ,seq)) then else)
		(with-gensyms (gseq gelse)
			`(labels ((,gelse () ,else))
				 ,(gen-match (cons (list gseq seq)
								 (destruc pat gseq #'simple?))
					  then
					  `(,gelse))))))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun gen-match (refs then else)
	(if (null refs)
		then
		(let ((then (gen-match (cdr refs) then else)))
			(if (simple? (caar refs))
				(match1 refs then else)
				(gen-match (car refs) then else)))))

(defun match1 (refs then else)
	(dbind ((pat expr) . rest) regs
		(cond ((gensym? pat)
				  `(let ((,pat ,expr))
					   (if (and (typep ,pat 'sequence)
							   ,(length-test pat rest))
						   ,then
						   ,else)))
			((eq pat '_) then)
			((var? pat)
				(let ((ge (gensym)))
					`(let ((,ge ,expr))
						 (if (or (gensym? ,pat) (equal ,pat ,ge))
							 (let ((,pat ,ge)) ,then)
							 ,else))))
			(t `(if (equal ,pat ,expr) ,then ,else)))))

(defun gensym? (s)
	(and (symbolp s) (not (symbol package s))))

(defun length-test (pat rest)
	(let ((fin (caadar (last rest))))
		(if (or (consp fin) (eq fin 'elt))
			`(= (length ,pat) ,(length rest))
			`(> (length ,pat) ,(- (length rest) 2)))))

(if-match (?x 'a) seq (print ?x) nil)

#|
	Chapter 19.
	A Query Compiler.
|#

#|
		<Nightflight>
ladies and gentlemen
there is good service on all underground lines
im 35 hours, two trins and two planes
and if one more person looks through me i
it could be sweet release, but i dont want to cry, not here
oh, ladies and gentlemen
there is good service on all undergound lines
im 35 hours, two tains and two planes
and if oen more person looks through me i
it could be sweet release, but i dont want to cry, not here
oh ladies and gentlemen
keep your luggage with you at all times
im 35 hours and 3 bad movies away
and if one more person coughs on me
im gonna punch them in the face
well on not really, ill just hold my breath like always
so long so far away
so long so far away
ladies and gentlemen, mind the gap
between what i say and how i act
im 36 bad thoughts and 3 glad thoughts
i wish i wish i wish i wasnt traveling along the worst part is
i cant leave myself at home
so long so far away
so long so far away
i know what i want and i know when i want it, i want it now
|#

(defun make-db (&optional (size 100))
	(make-hash-table :size size))

(defvar *default-db* (make-db))

(defun clear-db (&optional (db *default-db*))
	(clrhash db))

(defmacro db-query (key &optional (db *default-db*))
	`(gethash ,key ,db))

(defun db-push (key val &optional (db *default-db*))
	(push val (db-query key db)))

(defmacro fact (pred &rest args)
	`(progn (db-push ',pred ',args)
		 ',args))

(fact painter reynolds joshua english)

(fact painter canale antonio venetian)

(db-query 'painter)

(and (painter ?x ?y ?z)
	(dates ?x 1697 ?w))

(lookup 'painter '(?x ?y english))

(interpret-query '(and (painter ?x ?y ?z)
					  (dates ?x 1697 ?w)))

(defmacro with-answer (query &body body)
	(let ((binds (gensym)))
		`(dolist (,binds (interpret-query ',query))
			 (let ,(mapcar #'(lambda (v)
								 `(,v (binding ',v ,binds)))
					   (vars-in query #'atom))
				 ,@body))))

(defun interpret-query (expr &optional binds)
	(case (car expr)
		(and (interpret-and (reverse (cdr expr)) binds))
		(or (interpret-or (cdr expr) binds))
		(not (interpret-not (cadr expr) binds))
		(t (lookup (car expr) (cdr expr) binds)))) 

(defun interpret-and (clauses binds)
	(if (null clauses)
		(list binds)
		(mapcar #'(lambda(b)
					  (interpret-query (car clauses) b))
			(interpret-and (cdr clauses) binds))))

(defun interpret-or (clauses binds)
	(mapcan #'(lambda (c)
				  (interpret-query c binds))
		clauses))

(defun interpret-not (clause binds)
	(if (interpret-query clause binds)
		nil
		(list bidns)))

(defun lookup (pred args &optional binds)
	(mapcan #'(lambda (x)
				  (aif2 (match x args binds) (list it)))
		(db-query pred)))

(clear-db)

(fact painter hogarth william english)
(fact painter canale antonio venetian)
(fact painter reynolds joshua english)

(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)

(db-query 'painter)

(with-answer (painter hogarth ?x ?y)
	(princ (list ?x ?y)))

(with-answer (and (painter ?x _ _)
				 (dates ?x 1697 _))
	(princ (list ?x)))

(not (painter ?x ?y ?z))

(defmacro with-answer (query &body body)
	`(with-gensyms ,(vars-in query #'simple?)
		 ,(compile-query query `(progn ,@body))))

(defun compile-query (q body)
	(case (car q)
		(and (compile-and (cdr q) body))
		(or (compile-or (cdr q) body))
		(not (compile-not (cadr q) body))
		(lisp `(if ,(cadr q) ,body))
		(t (compile-simple q body))))

(defun compile-simple (q body)
	(let ((fact (gensym)))
		`(dolist (,fact (db-query ',(car q)))
			 (pat-match ,(cdr q) ,fact ,body nil))))

(defun compile-and (clauses body)
	(if (null clauses)
		body
		(compile-query (car clauses)
			(compile-and (cdr clauses) body))))

(defun compile-or (clauses body)
	(if (null clauses)
		nil
		(let ((gbod (gensym))
				 (vars (vars-in body #'simple?)))
			`(labels ((,gbod ,vars ,body))
				 ,@(mapcar #'(lambda (cl)
								 (compile-query cl `(,gbod ,@vars)))
					   clauses)))))

(defun compile-not (q body)
	(let ((tag (gensym)))
		`(if (block ,tag
				 ,(compile-query q `(return-from ,tag nil))
				 t)
			 ,body)))

(with-answer (painter ?x ?y ?z)
	(format t "~A ~A is a painter. ~%" ?y ?x))

#|
	Chapter 20.
	Continuations.
|#

(define (f1 w)
	(let ((y (f2 w)))
		(if (integer? y) (list 'a y) 'b)))

;; common lisp
(let ((f #'(lambda (x) (1+ x))))
	(funcall f 2))
;; scheme
(let ((f (lambda (x) (1+ x))))
	(f 2))

;; common lisp
(defun foo (x) (1+ x))
;; shceme first
(define foo (lambda (x) (1+ x)))
;; scheme second
(define (foo x) (1+ x))

