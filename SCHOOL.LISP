;1_3
(defun example1 ()
  (/ (* 7
        (+ 1 5.8))
     (* 4
        (- 2.51 2.34))))

;1_10
(defun if-is-not-a-function ()
  (if t t (/ 1 0)))

;2_3
(defun power2 (a)
  (* a a))

;2_4
(defun power3 (a)
  (* a a a))

(defun power4 (a)
  (* a a a a))

(defun power5 (a)
  (* a a a a a))

(defun power-i (a i)
  (if (<= i 1)
      (if (= i 0)
          1
        a)
    (* a (power-i a (- i 1)))))

;2_5
(defun hypotenuse (a b)
  (sqrt (+ (* a a)
           (* b b))))

;2_6
(defun absolute-value (a)
  (* a (if (< a 0) 
           -1 
         1)))

;2_7
(defun sgn (a)
  (if (= a 0) 
      0 
    (if (< a 0)
        -1
      1)))

;2_8
(defun maximum (a b)
  (if (<= a b) 
      b
    a))

(defun minimum (a b)
  (if (<= a b) 
      a
    b))

;2_9
(defun my-positive-p (a)
  (if (<= a 0)
      nil
    t))

(defun my-negative-p (a)
  (if (<= a 0)
      t
    nil))

;2_11
(defun trianglep (a b c)
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

;2_12
(defun heron (a b c)
  (let ((s (/ (+ a b c)
              2)))
    (sqrt (* s
             (- s a)
             (- s b)
             (- s c)))))

;2_13
(defun heron-cart (A-x A-y B-x B-y C-x C-y)
  (let ((a (sqrt (+ (power2 (- A-x B-x))
                    (power2 (- A-y B-y)))))
        (b (sqrt (+ (power2 (- B-x C-x))
                    (power2 (- B-y C-y)))))
        (c (sqrt (+ (power2 (- A-x C-x))
                    (power2 (- A-y C-y))))))
    (heron a b c)))


;3_5
(defun ellipse-area (a b)
  (* pi a b))

;3_5 recursion
(defun ellipse-area-rec (a b)
  (if (eql b t) (ellipse-area-rec a a)
    (* pi a b)))

;3_5 eql no recursion
(defun ellipse-area-classic (a b)
  (if (eql b t) (* pi a a)
    (* pi a b)))

;3_6 recursion
(defun squarep-iter (n i)
  (if (> n i) 
      (if (= n (* i i))
          t
        (squarep-iter n (+ i 1)))
    nil))

(defun squarep (n)
  (squarep-iter n 1))

;3_6 using cond
(defun squarep-cond-rec (n i)
  (cond ((< n (* i i)) nil)
        ((= n (* i i)) t)
        (t (squarep-cond-rec n (+ i 1)))))

(defun squarep-cond (n)
  (squarep-cond-rec n 1))

;3_6 logical
(defun squarep-log-rec (n i)
  (and (>= n (* i i))
       (or (= n (* i i))
           (squarep-log-rec n (+ i 1)))))

(defun squarep-log (n)
  (squarep-log-rec n 1))

;3_8
(defun my-gcd (a b)
 (if (= b 0)
     a
   (my-gcd b (rem a b))))

;3_9
(defun approx-= (a b epsilon)
  (<= (abs (- a b)) epsilon))

(defun heron-sqrt-iter (a b epsilon)
  (let ((b (/ (+ b (/ a b)) 2)))
    (if (approx-= a (* b b) epsilon)
        (* b 1.0)
      (heron-sqrt-iter a b epsilon))))

(defun heron-sqrt (a epsilon)
  (heron-sqrt-iter a 1 epsilon))

;3_11
(defun sum-of-n-elements-naive (a b)
  (/ (* (1+ (- b a))
        (+ a b))
     2))

;3_12
(defun sum-of-n-elements (a b)
  (if (> a b)
      0
    (+ a (sum-of-n-elements (1+ a) b))))

;3_13
(defun digit-count (n)
  (if (and (<= 0 n)
           (<= n 10))
      1
    (+ 1 (digit-count (/ n 10)))))

(defun digit (n i)
  (if (< i (digit-count n))
      (- (floor (/ n
                   (power-i 10
                            i)))
         (* (floor (/ n
                      (power-i 10
                               (+ i 1))))
            10))
    nil))

(defun sum-of-digits (n i d ir)
  (if (= i d)
      ir
    (sum-of-digits n (1+ i) d (+ ir (digit n i)))))

(defun is-divisible-nine-iter (n x i)
  (cond ((< n (* i 9)) nil)
        ((= (* i 9) n) t)
        (t (is-divisible-nine-iter n x (1+ i)))))

(defun is-divisible-nine (n)
  (is-divisible-nine-iter n
                          (sum-of-digits n
                                         0
                                         (digit-count n)
                                         0)
                          1))

;3_14
(defun leibniz-iter (pseudo-pi x e)
  (if (approx-= pi (* 4 pseudo-pi) e)
      (* 4.0 pseudo-pi)
    (let ((x (* (/ x
                   (abs x)
                   -1)
                (+ 2 (abs x))))
          (pseudo-pi (+ pseudo-pi
                        (/ 1 x))))
    (leibniz-iter pseudo-pi x e))))

(defun leibniz (e)
  (leibniz-iter 1 -3 e))

;4_2
;n=0 --> ir
;
;n%2=1 -> a**2 n/2 ir
;
;n%2=0 --> a n-1 a*ir
(defun fast-power-iter (a n ir)
  (cond((= n 0) ir)
       ((not (evenp n)) (fast-power-iter a (- n 1) (* a ir)))
       ((evenp n) (fast-power-iter (power2 a) (/ n 2) ir)))); teoreticky aj t lebo vzdy ked nie je %2 nie je %2 yeah

(defun fast-power (a n)
  (fast-power-iter a n 1))

;4_3
(defun dividesp (a b)
  (if (= (rem b a) 0)
      t
    nil))

;4_4
(defun dividesp-iter (a b)
  (cond ((= b 0) t)
        ((< b 0) nil)
        (t (dividesp-iter a
                          (- b a))))
  )

(defun dividesp1 (a b)
  (dividesp-iter (abs a) (abs b)))

;4_5
(defun primep-iter (a b)
  (if (= b 1) 
      t
    (and (not (= (rem a b)
                 0))
         (primep-iter a (1- b)))))

(defun primep (a)
  (primep-iter a (1- a)))

(defun primep1-iter (a b)
  (cond ((= b 1) t)
        ((= (rem a b) 0) nil)
        (t (primep1-iter a (1- b)))))

(defun primep1 (a)
  (cond ((<= a 1) nil)
        (t (primep1-iter a (1- a)))))

;4_6
(defun perfectp-iter (b a)
  (cond ((= b a) 0)
        (t (+ (if (= (rem a b)
                      0)
                  b
                0)
              (perfectp-iter (1+ b) a)))))

(defun perfectp (a)
  (if (= a (perfectp-iter 1 a))
      t
    nil))

;4_7
(defun pascal (y x)
  (cond ((< y x) 0)
        ((= x 0) 1)
        ((= y 0) 1)
        (t (+ (pascal (1- y) (1- x))
              (pascal (1- y) x)))))

;4_8
(defun sum-of-squares-p (a)
  (sum-of-squares-p-iter a a))

(defun sum-of-squares-p-iter (a b)
  (cond ((= a 0) t)
        ((or (< a 0)
             (< b 0)) nil)
        (t (or (sum-of-squares-p-iter a
                                      (1- b))
               (sum-of-squares-p-iter (- a
                                         (* b b))
                                      (1- b))))))

;4_9
(defun fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 2)) (fib (- n 1))))))

(defun fib2-iter (a b i n)
  (cond ((= n 0) 0)
        ((= n i) b)
        ((fib2-iter b
                    (+ a b)
                    (1+ i)
                    n))))

(defun fib2 (n)
  (fib2-iter 0 1 1 n))

;5_1
(defun right-triangle-p (A B C)
  (let ((ab2 (+ (expt (- (car A) (car B)) 2)
               (expt (- (cdr A) (cdr B)) 2)))
        (ac2 (+ (expt (- (car A) (car C)) 2)
               (expt (- (cdr A) (cdr C)) 2)))
        (bc2 (+ (expt (- (car B) (car C)) 2)
               (expt (- (cdr B) (cdr C)) 2))))
    (or (= ab2 (+ ac2 bc2))
        (= ac2 (+ ab2 bc2))
        (= bc2 (+ ab2 ac2)))
))

;5_2
(defun mid-point (A B)
  (cons (/ (+ (car A) (car B))
           2)
        (/ (+ (cdr A) (cdr B))
           2)))

(defun op-vertex (A B)
  (cons (- (* 2 (car B))
           (car A))
        (- (* 2 (cdr B))
           (cdr A))))

;5_3
(defun fraction (n d)
  (let ((div (gcd n d)))
    (cons (/ n div) (/ d div))))

(defun fraction- (A B)
  (fraction (- (* (car A) (cdr B))
               (* (cdr A) (car B)))
            (* (cdr A)
               (cdr B))))

;5_5
(defun interval (a b)
  (cond ((> a b) nil)
        ((= a b) (cons a nil))
        (t (cons a (interval (1+ a) b)))))

(defun lower-bound (x)
  (car x))

(defun upper-bound (x)
  (if (eql (cdr x) nil) ;bruh eql coz = je len na cisla bruh
      (car x)
    (upper-bound (cdr x))))

(defun number-in-interval-p (n X)
  (cond ((eql n (car X)) t)
        ((eql nil (cdr X)) nil)
        (t (number-in-interval-p n (cdr X)))))

(defun interval-intersection (A B)
  (cond ((number-in-interval-p (lower-bound A) B)
         (cons (lower-bound A)
               (interval-intersection (cdr A) B)))
        ((eql A nil) nil)
        (t (interval-intersection (cdr A) B))))

;5_6
(defun practice-5-6 ()
  (let ((pair (cons 3 4)))
    (cons 1 (cons pair (cons pair 2)))))


;(let ((pair) (cons 3 4)))
;(cons 1 (cons pair (cons pair 2)))
;

(defun practice-5-6-2 ()
  (let ((pair-1 (cons 3 (cons 6 nil)))
        (pair-2 (cons 4 (cons 9 nil))))
    (cons pair-1 pair-2)))

(defun practice-5-6-3 ()
  (cons (cons 3
              (cons 6
                    nil))
        (cons 4
              (cons 9
                    nil))))

;5_7

;(defun proper-list-p-iter (X n)
;  (cond ((and (eql (cdr X) nil)
;              (= n 1)) t)
;        ((= n 0) nil)
;        (t (proper-list-p-iter (cdr X) (1- n)))))
;
;(defun proper-list-p (X)
;  (if (and (eql X nil)
;           (= 0 (length X)))
;      t
;  (proper-list-p-iter X (length X))))

;(defun proper-list-p (X)
;  (eql (last X 0) nil))
;proper list je dole od krupku

;5_8
(defun my-make-list-iter (i elem)
  (if (= i 0)
      nil
    (cons elem
          (my-make-list-iter (1- i) elem))))

(defun my-make-list (length elem)
  (my-make-list-iter length elem))

;5_9
(defun make-ar-seq-list (a d i)
  (if (= i 0)
      nil
    (cons a
          (make-ar-seq-list (+ a d) d (1- i)))))

;5_10 ar-seq-iter
(defun make-ar-seq-list-iter (a d i ir)
  (cond ((= i 0) (cons a ir))
        ((< i 0) ir)
        (t (make-ar-seq-list-iter a
                                  d
                                  (1- i)
                                  (cons (+ a (* i d))
                                        ir)))))

(defun make-ar-seq-list2 (a d i)
  (make-ar-seq-list-iter a
                         d
                         (- i 1)
                         nil))

;5_11
(defun make-geom-seq-list (a q i)
  (if (= i 0)
      nil
    (cons a
          (make-geom-seq-list (* a q) q (1- i)))))

;5_12 geom-seq-iter
(defun make-geom-seq-list-iter (a q i ir)
  (cond ((= i 0) (cons a ir))
        ((< i 0) ir)
        (t (make-geom-seq-list-iter a
                                    q
                                    (1- i)
                                    (cons (* a (expt q i))
                                          ir)))))

(defun make-geom-seq-list2 (a q i)
  (make-geom-seq-list-iter a
                           q
                           (1- i)
                           nil))

;nuh uh pisomka recc asi A som bol
(defun delitelne-i (a b)
  (cond ((= a 0) t)
        ((< a 0) nil)
        (t (delitelne-i (- a b) b))))

;;;;;;;
(defun proper-list-p (x)
  (or (null x)
      (and (consp x)
           (proper-list-p (cdr x)))))

(defun my-nth (n list) 
  (if (= n 0)
      (car list)
    (my-nth (- n 1) (cdr list))))

(defun my-nthcdr (n list) 
  (if (= n 0)
      list
    (my-nthcdr (- n 1) (cdr list))))

(defun append-2 (list1 list2)
  (if (null list1)
      list2
    (cons (car list1) 
          (append-2 (cdr list1) list2))))

(defun my-revappend (list1 list2)
  (if (null list1)
      list2
    (my-revappend (cdr list1)
                  (cons (car list1) list2))))

(defun my-reverse (list)
  (my-revappend list ()))

(defun even-conses (list)
  (if (null list)
      ()
    (cons (car list) (odd-conses (cdr list)))))

(defun odd-conses (list)
  (if (null list)
      ()
    (even-conses (cdr list))))

(defun scale-list (list factor) 
  (if (null list)
      ()
    (cons (* factor (car list))
          (scale-list (cdr list) factor))))

(defun sum-lists-2 (list1 list2) 
  (if (null list1)
      ()
      (cons (+ (car list1) (car list2))
            (sum-lists-2 (cdr list1) (cdr list2)))))

(defun elementp (x list)
  (and (not (null list))
       (or (eql x (car list))
           (elementp x (cdr list)))))

(defun my-subsetp (list1 list2)
  (or (null list1)
      (and (elementp (car list1) list2)
           (my-subsetp (cdr list1) list2))))

(defun my-intersection (list1 list2)
  (cond ((null list1) ())
        ((elementp (car list1) list2) 
         (cons (car list1)
               (intersection (cdr list1) list2)))
        (t (intersection (cdr list1) list2))))

(defun subsets (list)
  (if (null list)
      (list ())
    (let ((cdr-subsets (subsets (cdr list))))
      (append-2 cdr-subsets
                (add-to-all (car list) cdr-subsets)))))

(defun add-to-all (elem list)
  (if (null list)
      ()
    (cons (cons elem (car list)) (add-to-all elem (cdr list)))))

(defun merge-sort (list) 
  (let ((list1 (even-conses list))
        (list2 (odd-conses list)))
    (if (or (null list) (null (cdr list)))
        list
      (merge-lists (merge-sort list1) (merge-sort list2)))))

(defun merge-lists (l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        ((<= (car l1) (car l2)) 
         (cons (car l1) (merge-lists (cdr l1) l2)))
        (t (cons (car l2) (merge-lists l1 (cdr l2))))))
;;;;;;;

;6_1
(defun last-pair (l)
  (if (null (cdr l))
      l
    (last-pair (cdr l))))

;6_2
(defun my-copy-list (l)
  (if (null l)
      nil
    (cons (car l) (my-copy-list (cdr l)))))

;6_3
(defun equal-lists-p (L1 L2)
  (or (and (null L1) (null L2))
      (and (eql (car L1) (car L2))
           (equal-lists-p (cdr L1) (cdr L2)))))

;6_4
(defun my-remove (x l)
  (cond ((null l) nil)
        ((eql x (car l)) (my-remove x (cdr l)))
        (t (cons (car l) (my-remove x (cdr l))))))

;6_5
(defun remove-nthcdr (n l)
  (if (= n 0)
      (cons nil (cdr l))
    (cons (car l) (remove-nthcdr (1- n) (cdr l)))))

;6_6
(defun each-other (l)
  (cond ((null l) nil)
        ((null (cddr l)) (cons (car l) nil))
        (t (cons (car l) (each-other (cddr l))))))

;6_7
(defun factorials-iter (n i last)
  (if (= n i)
      nil
    (let ((last (* last i)))
      (cons last (factorials-iter n (1+ i) last)))))

(defun factorials (n)
  (cons 1 (factorials-iter n 1 1)))

;6_8
(defun fib-list-iter (a b n)
  (if (= n 0)
      nil
    (cons b (fib-list-iter b (+ a b) (1- n)))))

(defun fib-list (n)
  (cons 0 (fib-list-iter 0 1 n)))

;6_9
(defun list-tails (l)
  (if (null l)
      (cons nil nil)
    (cons l (list-tails (cdr l)))))

;6_10
(defun list-sum (l)
  (if (null l)
      0
    (+ (car l) (list-sum (cdr l)))))

;6_12
(defun subtract-lists-2 (a b)
  (if (and (null a)
           (null b))
      nil
    (cons (- (car a) (car b))
          (subtract-lists-2 (cdr a) (cdr b)))))

;6_13
(defun scalar-product (a b)
  (if (and (null a)
           (null b))
      0
    (+ (* (car a) (car b))
       (scalar-product (cdr a) (cdr b)))))

;6_14
(defun list-sum-pow2 (a)
  (if (null a)
      0
    (+ (expt (car a) 2)
       (list-sum-pow2 (cdr a)))))

(defun vector-length (a)
  (sqrt (list-sum-pow2 a)))

;6_15
(defun first-index (x l)
  (first-index-iter l (length l) 0 x))

(defun first-index-iter (l n i x)
  (cond ((null l) nil)
        ((eql x (car l)) i)
        (t (first-index-iter (cdr l) n (1+ i) x))))

(defun my-remove-duplicates-iter (l lf)
  (cond ((null l) 
         lf)
        ((null (first-index (car l) lf))
         (my-remove-duplicates-iter (cdr l)
                                    (cons (car l)
                                          lf)))
        (t (my-remove-duplicates-iter (cdr l) lf))))

(defun my-remove-duplicates (l)
  (my-remove-duplicates-iter l nil))

;6_16
(defun my-union-iter (a x b)
  (cond ((and (null a)
              (null b)) 
         x)
        ((null a)
         (my-union-iter b x a))
        ((null (first-index (car a) x))
         (my-union-iter (cdr a) (cons (car a) x) b))
        (t
         (my-union-iter (cdr a) x b))))

(defun my-union (a b)
  (my-union-iter a nil b))

;6_17
;equal-lists-p ? uz sa robila v ulohe 2 ci kolko

;6_18
(defun mnozina (l)
  (merge-sort (my-remove-duplicates l)))

;6_19
(defun flatten-iter (l x)
  (cond ((null l)
         x)
        ((consp (car l))
         (flatten-iter (cdr l)
                       (flatten-iter (car l)
                                     x)))
        (t 
         (flatten-iter (cdr l) 
                       (cons (car l) x)))))

(defun flatten (l)
  (mnozina (flatten-iter l nil)))

;6_20
(defun deep-reverse-iter (l x)
  (cond ((null l)
         x)
        ((consp (car l))
         (deep-reverse-iter (cdr l)
                            (cons (deep-reverse-iter (car l) nil)
                                  x)))
        (t 
         (deep-reverse-iter (cdr l) 
                            (cons (car l) x)))))

(defun deep-reverse (l)
  (deep-reverse-iter l nil))

;07.lisp

(defun list-inc (list)
  (mapcar #'1+ list))

(defun log-10 (x)
  (log x 10))

(defun list-log-10 (list)
  (mapcar #'log-10 list))

;(defun sum-lists-2 (list1 list2) 
;  (mapcar #'+ list1 list2))

(defun findt (x list test) 
  (cond ((null list) nil)
        ((funcall test x (car list)) (car list)) 
        (t (findt x (cdr list) test))))

(defun my-find-if (pred list) 
  (cond ((null list) nil)
        ((funcall pred (car list)) (car list)) 
        (t (my-find-if pred (cdr list)))))

;(defun my-remove (x list) 
;  (cond ((null list) '())
;        ((eql x (car list)) 
;         (my-remove x (cdr list)))
;        (t (cons (car list) 
;                 (my-remove x (cdr list))))))

(defun removet (x list test) 
  (cond ((null list) '())
        ((funcall test x (car list)) 
         (removet x (cdr list) test)) 
        (t (cons (car list) 
                 (removet x (cdr list) test)))))

(defun my-remove-if (pred list) 
  (cond ((null list) '())
        ((funcall pred (car list)) 
         (my-remove-if pred (cdr list))) 
        (t (cons (car list) 
                 (my-remove-if pred (cdr list))))))

(defun foldr (fun list init)
  (if (null list)
      init
    (funcall fun 
             (car list) 
             (foldr fun (cdr list) init))))

;(defun append-2 (list1 list2) 
;  (foldr #'cons list1 list2))

(defun my-remove-2 (x list) 
  (labels ((rem (el accum)
             (if (eql x el) accum (cons el accum))))
    (foldr #'rem list '())))

;(defun scalar-product (list1 list2) 
;  (if (null list1)
;      '()
;    (+ (* (car list1) (car list2))
;       (scalar-product (cdr list1) (cdr list2)))))


;(defun scalar-product (list1 list2)
;  (foldr #'+ (mapcar #'* list1 list2) 0))

;7_1a
(defun fixpoint (f epsilon &rest n)
  (labels ((approx-= (a b epsilon)
             (<= (abs (- a b)) epsilon))
           
           (heron-sqrt-fixpoint (a b epsilon)
             (let ((b (/ (+ b (/ a b)) 2)))
               (if (approx-= a (* b b) epsilon)
                   (* b 1.0)
                 (heron-sqrt-fixpoint a b epsilon))))
           
           (cos-fixpoint (x epsilon)
             (let ((y (cos x)))
               (if (approx-= x y epsilon)
                   y
                 (cos-fixpoint y epsilon)))))
    (cond ((eql (quote cos) f)
           (cos-fixpoint 0 epsilon))
          ((eql (quote sqrt) f)
           (heron-sqrt-fixpoint (car n) 1 epsilon)))))

;7_1b
(defun my-length-2 (list)
  (- (foldr #'+ (mapcar #'1+ list) 0)
     (foldr #'+ list 0)))

(defun my-make-list-2 (length elem)
  (labels ((iter (length elem)
             (if (= 0 length)
                 '()
               (cons elem (iter (1- length) elem)))))
    (iter length elem)))

;7_1c
(defun add-to-all-2 (elem list)
  (mapcar #'cons
          (my-make-list-2 (my-length-2 list)
                          elem)
          list))

(defun merge-sort-2 (list f) 
  (let ((list1 (even-conses list))
        (list2 (odd-conses list)))
    (if (or (null list) (null (cdr list)))
        list
      (merge-lists-2 (merge-sort-2 list1 f) (merge-sort-2 list2 f) f))))

(defun merge-lists-2 (l1 l2 f)
  (cond ((null l1) l2)
        ((null l2) l1)
        ((funcall f (car l1) (car l2)) 
         (cons (car l1) (merge-lists-2 (cdr l1) l2 f)))
        (t (cons (car l2) (merge-lists-2 l1 (cdr l2) f)))))

;7_2
(defun my-member (a list f)
  (cond ((null list) '())
        ((not (eql (symbolp a)
                   (symbolp (car list))))
         (my-member a (cdr list) f))
        ((funcall f a (car list)) list)
        (t (my-member a (cdr list) f))))
;idk preco ale pri "a to dava error aj ked 
;pozeram ci su to obidvoje symboly

;7_3
(defun my-mapcar-2 (fun list)
  (if (null list)
      '()
    (foldr #'cons 
           (list (funcall fun (car list)))
           (my-mapcar-2 fun (cdr list)))))

;7_4
(defun foldl (fun list init)
  (labels ((deep-reverse-list-call (list)
             (deep-reverse-iter list nil))
           
           (foldl-in (fun list init)
             (if (null list)
                 init
               (funcall fun 
                        (car list) 
                        (foldl-in fun (cdr list) init)))))
    (foldl-in fun (deep-reverse-list-call list) init)))

(defun revcons (x y)
  (cons y x))

;7_5
(defun arithmetic-mean (&rest numbers)
  (/ (apply #'+ numbers)
     (length numbers)))

;7_6
#|
(defun equal-lists-p (&rest lists)
  (or (and (null L1) (null L2))
      (and (eql (car L1) (car L2))
           (equal-lists-p (cdr L1) (cdr L2)))))

(defun equal-lists-p-2 (&rest lists)
  (labels ((eq-lists-p (lists)
             (cond ((foldr #'eql (all-consp lists) t)
                    (eq-lists-p (minus-one-car lists)))
                   ()
                   ()))
           (minus-one-car (lists)
             (if (null lists)
                 nil
               (cons (caar lists)
                     (minus-one-car (cdr lists)))))
           (all-consp (lists)
             (if (null lists)
                 nil
               (consp (caar lists)
                     (minus-one-car (cdr lists)))))
           (eql-inside (p-list)
             (or (foldr #'eql p-list nil)
                 (foldr #'eql p-list t)))
           (eql-inside-x (list x)
             ())))
    (minus-one-car lists)))
|#

(defun equal-lists-p-2 (&rest lists)
  (labels ((eq-lists-p (lists)
             (and (equal-lists-p-two-lists
                   (my-make-list-2 (length lists) (length (car lists)))
                   (len-fr-elem-lists lists #'length))
                  (equal-lists-p-two-lists
                   (my-make-list-2 (length lists) (car (car lists)))
                   (len-fr-elem-lists lists #'car))
                  (minus-one-car lists)))
           (minus-one-car (lists)
             (if (equal-lists-p-two-lists
                     (my-make-list-2 (length lists) nil)
                     lists)
                 t
               (eq-lists-p (minus-one-car-extra lists))))
           (minus-one-car-extra (lists)
             (cond ((null lists) nil)
                   (t (cons (cdar lists) (minus-one-car-extra (cdr lists))))))
           (len-fr-elem-lists (lists fun)
             (if (null lists)
                 nil
               (cons (funcall fun (car lists))
                     (len-fr-elem-lists (cdr lists) fun))))
           (equal-lists-p-two-lists (L1 L2)
             (or (and (null L1) (null L2))
                 (and (eql (car L1) (car L2))
                      (equal-lists-p (cdr L1) (cdr L2))))))
    (eq-lists-p lists)))

;ta 7_7 mapcar tak podobne ako tu hore ze prechadza naraz tie listy 
;a ak jeden skoncill tak tam drzi nil a pri napr + tak namiesto nil pocita s nulou a * s 1
;teda to moze brat tu funkciu #"+ a #"* doslova ako hodnotu za nil lebo to je 1 a 0