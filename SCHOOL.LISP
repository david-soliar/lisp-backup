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
(defun sum-of-n-elements (a b)
  (if (> a b)
      0
    (+ a (sum-of-n-elements (1+ a) b))))

;3_14
