
;TREE-CONTAINS takes an atom N and a list TREE 
;and returns a bool depending on whether N exists in TREE.

;First, I check if TREE is empty. If it is, 
;then N cannot exist in it, so return NIL.
;Next, I check whether the TREE is a single number. 
;If it is a single number, then if it is N, return T. 
;Otherwise, we return NIL.
;Finally if TREE is a list, 
;recurse through all three branches of the TREE.
(DEFUN TREE-CONTAINS (N TREE)
    (COND ((NULL TREE) NIL)  
          ((ATOM TREE)
           (COND ((= TREE N) T)
           (t NIL)))
          (t (OR (TREE-CONTAINS N (FIRST TREE)) (TREE-CONTAINS N (SECOND TREE)) (TREE-CONTAINS N (THIRD TREE))))))

;TREE-MAX takes a TREE and returns an atom 
;that represents the maximum value in the TREE.

;First, I check if TREE is empty. If it is, 
;return NIL since you can't have a max of an empty list.
;Next if the TREE only has one element, 
;then return that element since it is the only option.
;Finally since it is an ordered tree from least to greatest, 
;the greatest value is always in the rightmost tree. 
;Therefore, we must recurse through the rightmost tree.
(DEFUN TREE-MAX (TREE)
    (COND ((NULL TREE) NIL) 
          ((ATOM TREE) TREE)
          (t (TREE-MAX(THIRD TREE)))))

;TREE-ORDER takes a TREE and returns a single list 
;with all the elements in the original TREE.

;First, we must check whether the TREE is empty. 
;If it is, return NIL.
;Then, we check whether the TREE is a single element. 
;If it is, return the element as a list.
;Finally if it is a list, then we recurse 
;through all the lists in TREE and APPEND them into one list.
(DEFUN TREE-ORDER (TREE)
    (COND ((NULL TREE) NIL)
          ((ATOM TREE) (CONS TREE NIL))
          (t(APPEND (TREE-ORDER(FIRST TREE)) (TREE-ORDER(SECOND TREE)) (TREE-ORDER(THIRD TREE))))))

;SUB-LIST takes a list L, an index called START 
;and a number called LEN. 
;It will return a sub-list of L 
;starting from the index START 
;and having a length of LEN.

;First, I check whether the list is empty. 
;If it is, then return NIL.
;Next, I check if START is not 0 and LEN is 0. 
;If both conditions are satisfied, 
;then return NIL since you can't have a 
;sub-list of length 0.
;Next if START is not 0, then we remove the current element 
;since it will not be a part of the sub-list. 
;We also decrement START.
;With these new values, we recurse through SUB-LIST.
;Next if the length is 0, return NIL.
;Finally if the length is not 0 and START is 0, 
;we attach the first element to a recursive 
;SUB-LIST containing the rest of L and 
;with the value of LEN decremented. 
;If LEN is 0, return NIL.
(DEFUN SUB-LIST (L START LEN)
       ;(COND ((OR (NULL L) (= LEN 0)) NIL)
       (COND ((NULL L) NIL)
             ((AND (NOT (= START 0)) (= LEN 0)) NIL)
             ((NOT (= START 0)) (SUB-LIST (CDR L) (- START 1) LEN))
             ((= LEN 0) NIL)
             (t (COND ((NOT (= LEN 0)) (CONS (CAR L) (SUB-LIST (CDR L) START (- LEN 1))))
                      (t NIL)))))

;SPLIT-LIST takes a list L and splits it into 
;two lists separated by their own sets of parentheses. 
;If the length of L is even, then 
;split the list 50/50. Otherwise, 
;give the extra element to the rightmost list.

;If the length of the list is even, 
;split the list perfectly in half.
;If the length of the list is odd, 
;give the leftmost list one less element than the rightmost list.
(DEFUN SPLIT-LIST (L)
       (COND ((EVENP (LENGTH L)) (LIST (SUB-LIST L 0 (/ (LENGTH L) 2)) (SUB-LIST L (/ (LENGTH L) 2) (/ (LENGTH L) 2))))
             (t (LIST (SUB-LIST L 0 (/ (- (LENGTH L) 1) 2)) (SUB-LIST L (/ (- (LENGTH L) 1) 2) (+ (/ (LENGTH L) 2) 1))))))

;BTREE-HEIGHT takes a binary tree TREE 
;and returns a number which represents the height of the TREE.

;If the TREE has one element, 
;then it has a height of 0 since it has no children nodes.
;If the length of the TREE is 1, then its height is 0. 
;This exists just in case a TREE is entered like this: (8).
;If the tree is empty, return 0.
;Otherwise, take the maximum height of the sub-trees 
;of the children nodes and add it to 1, 
;which represents the current level.
(DEFUN BTREE-HEIGHT (TREE)
       (COND ((ATOM TREE) 0)
             ((= (LENGTH TREE) 1) 0)
             ((NULL TREE) 0)
             (t (COND ((OR (< (BTREE-HEIGHT (FIRST TREE)) (BTREE-HEIGHT (SECOND TREE))) (= (BTREE-HEIGHT (FIRST TREE)) (BTREE-HEIGHT (SECOND TREE)))) (+ (BTREE-HEIGHT (SECOND TREE)) 1))
                (t (+ (BTREE-HEIGHT (FIRST TREE)) 1))))))

;LIST2BTREE takes a list of elements called LEAVES 
;and returns a list of lists that group the LEAVES into a 
;representation of a binary tree.

;If there is one terminal node, 
;then return that terminal node.
;If there are two terminal nodes, 
;then group them together in a list using the SUB-LIST function.
;If there are three terminal nodes, 
;attach the first leaf to a list containing the latter two leaves.
;If there are four terminal nodes, 
;attach two lists that each contain two leaves.
;If there are more than four terminal nodes in LEAVES, 
;then attach two lists. 
;The leftmost list contains a recursive call that acts
;on the elements from 0 to the rest of the list 
;that is not the last 4 elements in LEAVES. 
;The rightmost list contains two lists
;with each list containing two elements. 
;These are the last 4 elements excluded in the leftmost list.
(DEFUN LIST2BTREE (LEAVES)
       (COND ((= (LENGTH LEAVES) 1) LEAVES)
             ((= (LENGTH LEAVES) 2) (SUB-LIST LEAVES 0 2))
             ((= (LENGTH LEAVES) 3) (LIST (CAR LEAVES) (SUB-LIST LEAVES 1 2)))
             ((= (LENGTH LEAVES) 4) (LIST (SUB-LIST LEAVES 0 2) (SUB-LIST LEAVES 2 2)))
             (t (LIST (LIST2BTREE (SUB-LIST LEAVES 0 (- (LENGTH LEAVES) 4))) (LIST (SUB-LIST LEAVES (- (LENGTH LEAVES) 4) 2) (SUB-LIST LEAVES (- (LENGTH LEAVES) 2) 2))))))

;BTREE2LIST takes a binary tree TREE and turns it into a list of numbers. 
;It is basically the inverse of LIST2BTREE.

;If the TREE is empty, return NIL 
;since you can't turn nothing into a list of numbers.
;If the TREE has one element, 
;return that one element as a list.
;Otherwise, APPEND two recursive calls 
;that take the first element of the list 
;and the rest of the list respectively.
(DEFUN BTREE2LIST (TREE)
       (COND ((NULL TREE) NIL)
             ((ATOM TREE) (cons TREE NIL))
             (t (APPEND (BTREE2LIST (CAR TREE)) (BTREE2LIST (CDR TREE))))))

;IS-SAME takes two LISP expressions and returns a bool 
;depending on whether the two expressions are identical or not.

;If both expressions are empty, 
;then return T since nothing equals nothing.
;If both expressions are atoms, 
;then check if the atoms are identical and return a boolean.
;If they are both lists, then recursively check 
;whether the first items of each lists are identical 
;and do the same with the rest of both lists. 
;If none of the above conditions match, 
;then they are different since they are different types of objects
;ie. ATOM is not a LIST or LIST is not equal to NULL.
(DEFUN IS-SAME (E1 E2)
       (COND ((AND (NULL E1) (NULL E2)) T)
             ((AND (ATOM E1) (ATOM E2)) (= E1 E2))
             ((AND (LISTP E1) (LISTP E2)) (AND (IS-SAME (CAR E1) (CAR E2)) (IS-SAME (CDR E1) (CDR E2))))
             (t NIL)))