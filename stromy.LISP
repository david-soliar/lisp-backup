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

#|
 (defun tree-find (value root)
   (if (eql (node-value root) value)
       root
     (tree-list-find value (node-children root))))
 (defun tree-list-find (value roots)
   (if (null roots)
       nil
     (let ((res (tree-find value (car roots))))
       (if (null res)
           (tree-list-find value (cdr roots))
         res))))
|#

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
;toto je origo dobre ako ma byt
;ale to prve s listami je lepsie ngl
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
;t-max funguje ako tak ale ano good
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

;tree height ale vracia to pocet uzlov k listu
;ale malo by to vracat pocet hran teda to co vracia ale minus 1
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

