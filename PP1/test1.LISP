;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2020/2021

;1
(defun my-signum (a)
  (if (> a 0)
      1
    (if (< a 0)
        -1
      0)))

;3
(defun point (x y)
  (cons x y))

(defun point-x (p)
  (car p))

(defun point-y (p)
  (cdr p))

(defun v1 (tr)
  (car tr))

(defun v2 (tr)
  (car (cdr tr)))

(defun v3 (tr)
  (cdr (cdr tr)))

(defun triangle (ax ay bx by cx cy)
  (cons (point ax ay)
        (cons (point bx by)
              (point cx cy))))

(defun triangle-center (tr)
  (let* ((points (list (v1 tr) (v2 tr) (v3 tr)))
         (x-p (mapcar (function point-x) points))
         (y-p (mapcar (function point-y) points)))
    (point (/ (foldr (function +) x-p 0) (length x-p))
           (/ (foldr (function +) y-p 0) (length y-p)))))

(defun foldr (fun list init)
  (if (null list)
      init
    (funcall fun (car list) (foldr fun (cdr list) init))))

;4
(defun sum-digits (n)
  (if (= n 0)
      0
    (let ((x (floor (/ n 10))))
      (+ (- n (* x 10))
         (sum-digits x)))))

(defun digit (n i)
  (- (floor (/ n (expt 10 i)))
     (* 10 (floor (/ n (expt 10 (1+ i)))))))

;5
;let cons

;6
(defun even-elements (list)
  (cond ((null list) nil)
        ((evenp (car list)) (cons (car list)
                                  (even-elements (cdr list))))
        (t (even-elements (cdr list)))))

;7
(defun my-set-intersection2 (a b)
  (cond ((null a) nil)
        ((elementp (car a) b) (cons (car a)
                                    (my-set-intersection2 (cdr a) b)))
        (t (my-set-intersection2 (cdr a) b))))

(defun elementp (x list)
  (cond ((null list) nil)
        ((eql x (car list)) t)
        (t (elementp x (cdr list)))))

;8
(defun my-set-intersection (a &rest list)
  (foldr (function my-set-intersection2)
         list
         a))

;9
(defun seq-interleave (s1 s2)
  (lambda (n)
    (if (evenp n)
        (funcall s1 (/ n 2))
      (funcall s2 (/ (1- n) 2)))))

(defun seq-to-list (seq n)
  (labels ((iter (i)
             (if (>= i n)
                 nil
               (cons (funcall seq i)
                     (iter (1+ i))))))
    (iter 0)))

;10
(defun find-path (tree val)
  (cond ((null tree) nil)
        ((eql val (car tree)) (list (car tree)))
        (t (find-paths (car tree) (cdr tree) val))))

(defun find-paths (prev children val)
  (if (null children)
      nil
    (let ((sub (find-path (car children) val)))
      (if (null sub)
          (find-paths prev (cdr children) val)
        (cons prev sub)))))

;2020/2021
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun max-path (tree)
  (cond ((null tree) nil)
        ((null (cdr tree)) tree)
        (t (cons (car tree)
                 (my-max-list (max-paths (cdr tree)))))))

(defun max-paths (children)
  (if (null children)
      nil
    (let ((sub (max-path (car children))))
      (append (list sub)
              (max-paths (cdr children))))))

(defun my-max-list (list)
  (cond ((null (cdr list)) (car list))
        ((< (length (car list)) (length (cadr list)))
         (my-max-list (cdr list)))
        (t (my-max-list (cons (car list)
                              (cddr list))))))

(defun tree-level (tree h level)
  (cond ((null tree) nil)
        ((< h level) nil)
        ((= level 0) (list (car tree)))
        (t (tree-levels (cdr tree) h (1- level)))))

(defun tree-levels (children h level)
  (if (null children)
      nil
    (let ((sub (tree-level (car children) h level)))
      (append sub
              (tree-levels (cdr children) h level)))))
