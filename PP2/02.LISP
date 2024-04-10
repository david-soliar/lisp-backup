;2
(defmacro and-2l (a b)
  (list 'if a (list 'if b t nil) nil))

(defmacro and-2s (a b)
  `(if ,a (if ,b t nil) nil))

(defmacro or-2l (a b)
  (list 'if a t (list 'if b t nil)))

(defmacro or-2s (a b)
  `(if ,a t (if ,b t nil)))

;3
(defmacro if-zerol (test a b)
  (list 'if (list 'eql 0 test) a b))

;4
(defmacro my-when (test &rest expressions)
  `(if ,test (progn ,@expressions) nil))

(defmacro my-unless1 (test &rest n)
  `(if ,test (progn ,@n) nil))

(defmacro my-unless2 (test &rest n)
  `(my-when ,test ,@n))

;5
(defmacro whenb (sym test &rest exp)
  `(when (setf ,sym ,test) ,@exp))

;6
(defmacro reverse-progn (&rest exp)
  `(progn ,@(reverse exp)))

;7
(defmacro my-let (binds body)
  `((lambda (,(caar binds))
     (if (null ',(cdr binds)) 
         ,body 
       (my-let ,(cdr binds) ,body)))
     ,(cadar binds)))

;quote je dolezity
