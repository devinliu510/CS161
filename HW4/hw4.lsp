;DISCLAIMER: I whipped up this code in one afternoon because I had midterms
;to study for. As a result, I did not implement any sort of optimization 
;such as forward searching. However, there are functions that I wrote that
;could be used to implement forward searching. I just didn't have the time 
;to do so. My code still works on the test cases, albeit the time to 
;complete the test cases is a bit long. I did try to eliminate as many
;function calls as possible to optimize the code.

;Takes a clause and evaluates it
;Returns T if a clause evaluates to T, NIL otherwise
;Example: checkClause '(1 2 -3) yields T while '(-1 -2 -3) yields NIL
;due to all values in the second clause being false
(defun checkClause (delta) 
    (COND ((NULL delta) NIL)
          ((ATOM delta) (>= delta 0))
          ;((= (LENGTH delta) 1) (>= (CAR delta) 0))
          (t (OR (checkClause (FIRST delta)) (checkClause (REST delta))))
    )
 )

 ;Takes a CNF delta and checks if it is solved
 ;Returns T if delta is solved
 ;Example: checkCNF '((1 2 3)(-1 2 3)(-1 -2 3)) yields T
 ;while '((1 2 3)(-1 2 3)(-1 -2 -3)) yields NIL 
 ;due to the last clause having all values = false
(defun checkCNF (delta)
     (COND ((ATOM delta) T)
           ((= (LENGTH delta) 1) (checkClause delta))                       
           (t (AND (checkCNF (LIST (FIRST delta))) (checkCNF (REST delta))))
      )
 )

 ;Takes a clause
 ;Finds the variable (value) in the clause and negates it
 ;Example: negation-helper '(1 2 3) 2 yields (1 -2 3)
 ;Example: negation-helper '(1 -2 3) 2 yields (1 2 3)
 (defun negation-helper (clause value)
    (COND ((NULL clause) NIL)
          ((ATOM clause)
               (COND ((OR (= clause value) (= (* clause -1) value)) (CONS (* clause -1) NIL))
                     (t (CONS clause NIL))
                )
           )
          ;((= (LENGTH clause) 1) 
               ;(COND ((= (CAR clause) value) (CONS (* (CAR clause) -1) NIL))
                     ;(t (CONS (CAR clause) NIL))
               ;)
           ;)
          (t (APPEND (negation-helper (FIRST clause) value) (negation-helper (REST clause) value)))
     )
 )

 ;Iterates through each clause in the CNF and calls negation-helper
 ;Example: negation '((1 2 3)(-1 2 3)(-1 -2 -3)) 3 
 ;yields ((1 2 -3)(-1 2 -3)(-1 -2 3))
(defun negation (delta value)
    (COND ;((ATOM delta) (CONS (negation-helper delta value) NIL))
          ((= (LENGTH delta) 1) (CONS (negation-helper delta value) NIL))
          (t (APPEND (negation (LIST (FIRST delta)) value) (negation (REST delta) value)))
     )
 )
 
 ;Originally going to use it to do forward checking
 ;to find the least constraining variable
 ;Finds how many occurences of var are in the CNF
 ;Example: pick-var-helper '((1 2 3)(-1 2 3)(-1 -2 -3)) 3 
 ;yields 3 as there are 3 occurences of the var 3
(defun pick-var-helper (delta var)
    (COND  ((NULL delta) 0)
           ((ATOM delta)
               (COND ((OR (= delta var) (= (* delta -1) var)) 1)
                     (t 0)
                )
           )
          (t (+ (pick-var-helper (FIRST delta) var) (pick-var-helper (REST delta) var)))
     )
 )

 ;Also originally going to be used for forward checking
;Determines whether v has already been assigned a value
;Returns T if already assigned a value
(defun search-vars-tried (vars-tried v)
    (COND  ((NULL vars-tried) NIL)
           ((ATOM vars-tried)
               (COND ((= vars-tried v) T)
                     (t NIL)
                )
           )
           (t (OR (search-vars-tried (FIRST vars-tried) v) (search-vars-tried (REST vars-tried) v)))
     )
 )
  
;Finds least constraining variable in forward checking  
;Not implemented in final code
 ;Picks variable with least constraint
;Returns (# of occurences, n)
;Returns (-1 1) if out of variables to choose from
(defun pick-var (delta n vars-tried) 
    (COND ((AND (search-vars-tried vars-tried n) (NOT (= n 1))) (pick-var delta (- n 1) vars-tried))
          ((AND (search-vars-tried vars-tried n) (= n 1)) (LIST -1 n))
          ((AND (NOT (search-vars-tried vars-tried n)) (= n 1)) (LIST (pick-var-helper delta n) n))
          (t 
               (COND ((= (FIRST (pick-var delta (- n 1) vars-tried)) -1) (LIST (pick-var-helper delta n) n))
                     (t 
                          (COND ((<= (pick-var-helper delta n) (FIRST (pick-var delta (- n 1) vars-tried)))
                                 (LIST (pick-var-helper delta n) n)
                                )
                                (t (pick-var delta (- n 1) vars-tried))
                           )
                     )
                )
           )
    )
 )
 
 ;finds solution using backtracking search 
 
 ;First if there is only one variable, then just try a negative and a positive
 ;value for it and see which one solves the CNF.
 ;If there is more than one variable, then do backtracking search.
 ;The backtracking search uses the fact that a promising path will have n-1
 ;variables since a non-promising path will return a NIL which would make the 
 ;length of the path not equal to n-1. If none of the paths has length n-1, 
 ;then return NIL as there is no solution.
 (defun backtrack (n delta) 
        (COND ((= n 1) 
                   (COND ((checkCNF (negation delta 1)) (CONS -1 NIL))
                         ((checkCNF delta) (CONS 1 NIL))
                         (t NIL)
                    )
               )
              (t 
					(let* ((try-neg (backtrack (- n 1) (negation delta n)))
						  (try-pos (backtrack (- n 1) delta))
						  )
						(COND ((= (LENGTH try-neg) (- n 1))
									(APPEND try-neg (CONS (* -1 n) NIL))
								)
								((= (LENGTH try-pos) (- n 1))
									(APPEND try-pos (CONS n NIL))
								)
								(t NIL)
						)
					)
               )
         )
  )

  ;wrapper function for backtrack function
(defun sat? (n delta)
	(COND ((= n 0) NIL)
          (t (backtrack n delta))
    )
)
