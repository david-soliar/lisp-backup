(defun tree-node (val children)
  (cons val children))

(defun node-value (node)
  (car node))

(defun node-children (node)
  (cdr node))

(defun tree-find (value tree)
  (cond ((null tree) nil)
        ((eql value (node-value tree)) tree)
        (t (tree-find-deeper value (node-children tree)))))

(defun tree-find-deeper (value trees)
  (cond ((null trees) nil)
        ((tree-find value (car trees))
         (tree-find value (car trees)))
        (t (tree-find-deeper value (cdr trees)))))

(defun tree-node-path (value root)
  (if (eql (node-value root) value)
      (list root)
    (let ((subpath (tree-list-node-path
                    value
                    (node-children root))))
      (if (null subpath)
          nil
        (cons root subpath)))))

(defun tree-list-node-path (value roots)
  (if (null roots)
      nil
    (let ((subpath (tree-node-path value (car roots))))
      (if (null subpath)
          (tree-list-node-path value (cdr roots))
        subpath))))

(defun foldr (fun list init)
  (if (null list)
      init
    (funcall fun (car list) (foldr fun (cdr list) init))))

(defun tree-values-dfs (tree)
  (cons (node-value tree)
        (foldr (function append)
               (mapcar (function tree-values-dfs)
                       (node-children tree))
               nil)))

(defun tree-values-bfs (tree)
  (tvb (list tree)))

(defun tvb (tree)
  (if (null tree)
      nil
    (append (mapcar (function node-value) tree)
            (tvb (foldr (function append)
                        (mapcar (function node-children) 
                                tree)
                        nil)))))

(defun binary-tree-node (val left-child right-child)
  (tree-node val (list left-child right-child)))

(defun left-child (node)
  (cadr node))

(defun right-child (node)
  (caddr node))

(defun node-children-bin (node)
  (remove nil (cdr node)))

; (a (c (b) (d)) (f (g (h)) (i)))
#|
(tree-node 'a 
           (list (tree-node 'c 
                            (list (tree-node 'b nil)
                                  (tree-node 'd nil)))
                 (tree-node 'f
                            (list (tree-node 'g 
                                             (list (tree-node 'h nil)))
                                  (tree-node 'i nil)))))
|#


(defun tree-sum (tree)
  (foldr (quote +) (tree-to-list tree) 0))

(defun tree-to-list (tree)
  (cons (node-value tree)
        (foldr (function append)
               (mapcar (function tree-to-list)
                       (node-children tree))
               nil)))

;(A (C (B) (D)) (F (G (H)) (I)))
#|
(tree-node 'a
           (tree-node (tree-node 'c
                                 (tree-node (tree-node 'b nil)
                                            (tree-node (tree-node 'd nil)
                                                       nil)))
                      (tree-node (tree-node 'f
                                            (tree-node (tree-node 'g
                                                                  (tree-node (tree-node 'h nil)
                                                                             nil))
                                                       (tree-node (tree-node 'i nil)
                                                                  nil)))
                                 nil)))
|#

(defun tv-b (tree)
  (if (null tree)
      nil
    (append (mapcar (function node-value) tree)
            (tv-b (mapcar (function tv-b) (node-children tree))))))

(defun ts (tree)
  (cond ((null tree) 0)
        ((consp tree) (+ (ts (car tree)) (ts (cdr tree))))
        (t tree)))

(defun t-max-help (tree1 tree2)
  (if (null tree2)
      (list tree1)
    (foldr (function append)
           (let ((new-paths (mapcar (function cons)
                                    (dupli-t tree1 (length tree2))
                                    (mapcar (function list) tree2))))
             (mapcar (function t-max-help)
                     new-paths
                     (mapcar (function node-children)
                             tree2)))
           nil)))

(defun t-max (tree)
  (t-max-help tree (node-children tree)))

(defun dupli-t (x n)
  (if (= n 0)
      nil
    (cons x (dupli-t x (1- n)))))

(defun test (a)
  (list (cons (car a) (cddr a))
        (cons (car a) (cadr a))))

(defun testx (a)
  (mapcar (function list)
          (dupli-t (car a) (length (cdr a)))
          (cdr a)))

(defun tree-height (tree)
  (if (null tree)
      0
      (1+ (max-of-subtree-heights (cdr tree)))))

(defun max-of-subtree-heights (subtrees)
  (if (null subtrees)
      0
      (max (tree-height (car subtrees))
           (max-of-subtree-heights (cdr subtrees)))))

(defun th (tree)
  (if (null (cdr tree))
      0
    (1+ (th-c (cdr tree)))))

(defun th-c (trees)
  (if (null trees)
      0
    (max (th (car trees))
         (th-c (cdr trees)))))

(defun random-number-p (pred)
  (let ((x (random 100)))
    (if (funcall pred x)
        x
      (random-number-p pred))))

(defun tree-max-paths (tree)
  (if (null tree)
      nil 
    (tree-max-paths-children (node-value tree)
                             (node-children tree))))

(defun tree-max-paths-children (value children)
  (labels ((add-value (x)
             (cons value x)))
    (if (null children)
        (list (list value))
      (let ((sub (tree-max-paths (car children))))
        (mapcar (function add-value)
                (foldr (function append)
                       sub
                       (tree-max-paths (cdr children))))))))

#|
(defun tmaxs (value children)
  (labels ((value-adder (x)
             (cons value x)))
    (if (null children)
         (list (list value))
       (let ((sub (tmax (car children))))
         (mapcar (function value-adder)
                 (append sub
                         (tmax (cdr children))))))))
|#

(defun my-append (a b)
  (cond ((null a) b)
        (t (my-append (cdr a) (cons (car a) b)))))
#|
(defun tmax (tree)
  (if (null tree)
      nil
    (labels ((value-adder (x)
               (cons (node-value tree) x)))
      (mapcar (function value-adder)
              (tmaxs (node-children tree))))))

(defun tmaxs (children)
  (let ((sub (tmax (car children))))
    (if (null sub)
        (list nil)
      (append sub
              (tmaxs (cdr children))))))
|#
(defun is-list (k)
  (null (cdr k)))

(defun tmax (tree)
  (if (null tree)
      nil
    (labels ((value-adder (x)
               (cons (node-value tree) x)))
      (mapcar (function value-adder)
              (tmaxs (node-children tree))))))

(defun tmaxs (children)
  (let ((sub (tmax (car children))))
    (if (null sub)
        (list nil)
      (append sub
              (if (null (cdr children))
                  nil
                (tmaxs (cdr children)))))))

;tree-height
(defun treeh (tree)
  (if (null tree)
      0
    (1+ (treehs (node-children tree)))))

(defun treehs (children)
  (cond ((null children) -1)
        ((null (cdr children)) (treeh (car children)))
        (t (max (treeh (car children))
                (treehs (cdr children))))))

;tree-sum
(defun tree-sum2 (tree)
  (if (null tree)
      0
    (+ (node-value tree)
       (tree-sums (node-children tree)))))

(defun tree-sums (children)
  (if (null children)
      0
    (+ (tree-sum2 (car children))
       (tree-sums (cdr children)))))

(defun siesta-u ()
  (binary-tree-node 5 
                    (binary-tree-node 
                     2 
                     (binary-tree-node 1 
                                       nil nil)
                     (binary-tree-node 3 
                                       nil (binary-tree-node 4 nil nil)))
                    (binary-tree-node 7 
                                      (binary-tree-node 6 nil nil) 
                                      (binary-tree-node 8 nil nil))))

(defun balancedp (bin-tree)
  (if (null bin-tree)
      t
    (balancedps (node-children-bin bin-tree))))

(defun balancedps (bin-children)
  (if (null bin-children)
      t
    (and (abs-height (left-child bin-children))
         (balancedps (right-child bin-children)))))

(defun abs-height (tree)
  (<= (- (treeh (left-child tree)) (treeh (right-child tree))) 1))

(defun sum-lists (list1 &rest lists)
  (apply #'mapcar #'+ list1 lists))

(defun fx (a b c d)
  (list a b c d))

(defun find-tree-path (tree value)
  (cond ((null tree) nil)
        ((eql value (car tree)) (cons value nil))
        (t (find-tree-paths (car tree) (cdr tree) value))))

(defun find-tree-paths (previous trees value)
  (if (null trees)
      nil
    (let ((sub (find-tree-path (car trees) value)))
      (if (null sub)
          (find-tree-paths previous (cdr trees) value)
        (cons previous sub)))))

(defun seq-interleave (l1 l2)
  (lambda (n) (if (evenp n)
                  (funcall l1 (/ n 2))
                (funcall l2 (/ (1- n) 2)))))

(defun stl (seq n)
  (labels ((iter (i)
             (if (>= i n)
                 nil
               (cons (funcall seq i)
                     (iter (1+ i))))))
    (iter 0)))

(defun my-set-intersection2 (a b)
  (cond ((null a) nil)
        ((elementp (car a) b) 
         (cons (car a) (my-set-intersection2 (cdr a) b)))
        (t (my-set-intersection2 (cdr a) b))))

(defun elementp (x list)
  (cond ((null list) nil)
        ((eql x (car list)) t)
        (t (elementp x (cdr list)))))

(defun my-set-intersection (list &rest lists)
  (foldr (function my-set-intersection2)
         lists
         list))

(defun even-elements (list)
  (labels ((iter (n list)
             (cond ((null list) nil)
                   ((evenp n) (cons (car list) (iter (1+ n) (cdr list))))
                   (t (iter (1+ n) (cdr list))))))
    (iter 0 list)))

(defun t1 (tree value)
  (cond ((null tree) nil)
        ((eql value (car tree)) (cons value nil))
        (t (t2 (car tree) (cdr tree) value))))

(defun t2 (prev children value)
  (if (null children)
      nil
    (let ((sub (t1 (car children) value)))
      (if (null sub)
          (t2 prev (cdr children) value)
        (cons prev sub)))))

(defun sum-digits (n)
  (if (= 0 n)
      0
    (let ((x (floor (/ n 10))))
      (+ (- n (* x 10))
         (sum-digits x)))))

;1
(defun jedna ()
  (funcall #'cons #'cons #'cons))

;2
(defun to-n (list)
  (if (null list)
      0
    (+ (* (car list) (expt 10 (1- (length list))))
       (to-n (cdr list)))))

;3
;(funcall (lambda (x) (1+ x)) 5)
;(let ((x 5)) (1+ x))

;5
(defun sums (x list)
  (labels ((iter (ir list)
             (cond ((and (= ir x)
                         (null list)) 1)
                   ((null list) 0)
                   ((= ir x) (+ 1 (iter (+ ir (car list)) (cdr list))
                                (iter ir (cdr list))))
                   (t (+ (iter (+ ir (car list)) (cdr list))
                         (iter ir (cdr list)))))))
    (+ (iter 0 (cdr list))
       (iter (car list) (cdr list)))))

;6
;(count '(1 (2 3) 4))
;2
(defun ccount (list)
  (cond ((consp list) (+ (ccount (car list)) (ccount (cdr list))))
        ((null list) 1)
        (t 0)))

;7
(defun eevery (list pred)
  (or (null list)
      (and (funcall pred (car list))
           (eevery (cdr list) pred))))

;9
(defun power2 (n)
  (* n n))

(defun seq-count-if (seq n pred)
  (labels ((iter (i)
             (cond ((>= i n) 0)
                   ((funcall pred (funcall seq i)) (+ 1 (iter (1+ i))))
                   (t (iter (1+ i))))))
    (iter 0)))

;10
(defun tree-level (tree level)
  (if (= level 0)
      (car tree)
    (remove-nil (tree-levels (cdr tree) (1- level)))))

(defun tree-levels (trees level)
  (if (= level 0)
      (mapcar (function car) trees)
    (tree-levels (mapcar (function cdr) trees)
                 (1- level))))

(defun remove-nil (list)
  (cond ((null list) nil)
        ((null (car list)) (remove-nil (cdr list)))
        (t (cons (car list)
                 (remove-nil (cdr list))))))

;1
(defun my-signum (n)
  (cond ((= n 0) 0)
        ((> n 0) 1)
        (t -1)))
;3
(defun trianglep (a b c)
  (labels ((is-t (x y z)
             (< x (+ y z))))
    (and (is-t a b c)
         (is-t b c a)
         (is-t c b a))))

;4
(defun euler (n)
  (labels ((iter (e i x)
             (if (> i n)
                 e
               (iter (+ e (/ 1 (* x i)))
                     (1+ i)
                     (* x i)))))
    (iter 1 1 1)))

;5
(defun lsum (x)
  (foldr (function +)
         x
         0))

(defun same-sums-p (list)
  (if (null list)
      nil
    (apply (function =) (mapcar (function lsum) list))))

;7
(defun rev (list x)
  (if (null list)
      x
    (rev (cdr list)
         (cons (car list) x))))

(defun my-reverse (list)
  (rev list nil))

;8
(defun prefix-to-infix (v)
  (if (consp v)
      (let ((n (length v)))
        (cond ((<= n 2) v)
              (t (add-fix (car v) (cdr v)))))
    v))

(defun add-fix (fix x)
  (if (null (cdr x))
      (cons (prefix-to-infix (car x))
            nil)
    (cons (prefix-to-infix (car x)) (cons fix (add-fix fix (cdr x))))))

;9
(defun seq-shift (seq k)
  (lambda (n)
    (if (< (+ n k) 0)
        0
      (funcall seq (+ n k)))))


(defun max-path (tree)
  (if (or (null tree) (null (cdr tree)))
      tree
    (cons (car tree)
          (m-s (cdr tree)))))

(defun m-s (children)
  (if (null children)
      nil
    (let ((sub (max-path (car children))))
      (if (null sub)
          nil
        (my-max-l sub
                  (m-s (cdr children)))))))

(defun my-max-l (a b)
  (if (< (length a) (length b))
      b
    a))

;;;;;;;;;;;;;;;;;;;;;;;;;
(defun max-path-node (tree)
  (if (or (null tree) (null (cdr tree)))
      tree
    (cons tree
          (m-s-node (cdr tree)))))

(defun m-s-node (children)
  (if (null children)
      nil
    (let ((sub (max-path-node (car children))))
      (if (null sub)
          nil
        (my-max-l sub
                  (m-s-node (cdr children)))))))

(defun find-val (tree value)
  (cond ((null tree) nil)
        ((eql value (car tree)) (cons (car tree) nil))
        (t (find-val-s (car tree) (cdr tree) value))))

(defun find-val-s (prev children value)
  (if (null children)
      nil
    (let ((sub (find-val (car children) value)))
      (if (null sub)
          (find-val-s prev (cdr children) value)
        (cons prev sub)))))
