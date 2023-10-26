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
    (sum-of-digits n (1+ i) d ir)))

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

(defun interval-intersection-iter (A B)
  (cond ((number-in-interval-p (lower-bound A) B)
         (cons (lower-bound A)
               (interval-intersection-iter (cdr A) B)))
        ((eql A nil) nil)
        (t (interval-intersection-iter (cdr A) B))))

(defun interval-intersection (A B)
  (if (or (number-in-interval-p (upper-bound A) B)
          (number-in-interval-p (lower-bound A) B))
      (interval-intersection-iter A B)
    nil))

;5_6
(defun practice-5-6 ()
  (cons 1 (cons (cons 3 4)
                (cons (cons 3 4)
                      (cons 2 nil)))))

;5_7
;(defun proper-list-p (X)
;  )

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

;5_11
(defun make-geom-seq-list (a q i)
  (if (= i 0)
      nil
    (cons a
          (make-geom-seq-list (* a q) q (1- i)))))

;5_12 geom-seq-iter