(defun my-revappend (l1 l2)
  (if (null l1)
      l2
    (my-revappend (cdr l1) (cons (car l1) l2))))

(defun even-conses (l)
  (if (null l)
      nil
    (cons (car l) (odd-conses (cdr l)))))

(defun odd-conses (l)
  (if (null l)
      nil
    (even-conses (cdr l))))

(defun last-pair (l)
  (if (not (consp (cdr l)))
      l
    (last-pair (cdr l))))

(defun my-copy-list (l)
  (cond ((null l) nil)
        ((not (consp (cdr l))) (cons (car l) (cdr l)))
        (t (cons (car l) (my-copy-list (cdr l))))))

(defun equal-lists-p (l1 l2)
  (cond ((and (null l1) (not (null l2))) nil)
        ((and (null l2) (not (null l1))) nil)
        ((and (null l1) (null l2)) t)
        ((eql (car l1) (car l2)) (equal-lists-p (cdr l1) (cdr l2)))
        (t nil)))

(defun elementp (x l)
  (cond ((null l) nil)
        ((eql (car l) x) t)
        (t (elementp x (cdr l)))))

(defun my-remove (x l)
  (cond ((null l) nil)
        ((eql x (car l)) (my-remove x (cdr l)))
        (t (cons (car l) (my-remove x (cdr l))))))

(defun remove-nthcdr (n l)
  (if (= n 0)
      (cons (car l) nil)
    (cons (car l) (remove-nthcdr (1- n) (cdr l)))))

(defun each-other (l)
  (cond ((null l) nil)
        ((null (cdr l)) (cons (car l) nil))
        (t (cons (car l) (each-other (cddr l))))))

(defun f-iter (n i ifact)
  (if (> i n)
      nil
    (cons ifact (f-iter n (1+ i) (* (1+ i) ifact)))))

(defun factorials (n)
  (f-iter n 0 1))

(defun fib-iter (a b i n)
  (if (> i n)
      nil
    (cons a (fib-iter b (+ a b) (1+ i) n))))

(defun fib-list (n)
  (fib-iter 0 1 0 n))

(defun list-tails (l)
  (if (null l)
      (cons nil nil)
    (cons l (list-tails (cdr l)))))

(defun list-sum (l)
  (if (null l)
      0
    (+ (car l)
       (list-sum (cdr l)))))

(defun scalar-product (l1 l2)
  (foldr (mapcar (quote *) l1 l2) (quote +) 0))

(defun foldr (list fun init)
  (if (null list)
      init
    (funcall fun (car list) (foldr (cdr list) fun init))))

(defun vector-length (l)
  (sqrt (foldr (mapcar (function (lambda (n) (* n n))) l)
               (function +)
               0)))

(defun my-union (a b)
  (cond ((null b) a)
        ((elementp (car b) a) (my-union a (cdr b)))
        (t (my-union (cons (car b) a) (cdr b)))))

(defun deep-reverse-iter (l new)
  (cond ((null l)
         new)
        ((consp (car l))
         (deep-reverse-iter (cdr l)
                            (cons (deep-reverse-iter (car l) nil) new)))
        (t (deep-reverse-iter (cdr l) (cons (car l) new)))))

(defun deep-reverse (l)
  (deep-reverse-iter l nil))

(defun flatten-iter (l new)
  (cond ((null l) new)
        ((consp (car l))
         (flatten-iter (cdr l) (flatten-iter (car l) new)))
        (t (flatten-iter (cdr l) (cons (car l) new)))))

(defun flatten (l)
  (flatten-iter l nil))
