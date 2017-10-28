
;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

;This function checks if there are any boxes (represented by int 2) left.
;If there are return NIL. Otherwise, return true.
(defun goal-test (s)
    (COND ((NULL s) T)
          ((ATOM s) 
               (COND ((isBox s) NIL)
                     (t T)
               )
           )
          (t (AND (goal-test (CAR s)) (goal-test (CDR s))))
      )
  );end defun

;This function gets the number of rows in the state.
(defun getRows (s)
    (COND ((NULL s) 0)
          (t (LENGTH s))
     )
 )

 ;This function gets the number of columns in the state.
(defun getCols (s)
    (COND ((NULL s) 0)
          (t (LENGTH (FIRST s)))
     )
 )

 ;This function goes through the row in question and finds 
 ;the element in the column specified by the variable c.  
(defun traverse-row (row c)
    (COND ((NOT (= c 0)) (traverse-row (CDR row) (- c 1)))
          (t (CAR row))
     )
 )

 ;This function gets the element specified by coordinates (c,r).
 ;First, you have to check if r or c are out of bounds.
 ;If they are, return a wall (int 1).
 ;If r is not 0, keep iterating downwards through the rows.
 ;If r is 0, traverse that row as the element is in that row.
(defun get-square (s c r)
    (COND ((<= (getRows s) r) 1)
          ((<= (getCols s) c) 1)
          ((NOT (= r 0)) (get-square (CDR s) c (- r 1)))
          (t (traverse-row (CAR s) c))
     )
 )

;This is pretty much the same as traverse-row except it replaces
;the element found in column c with the value v. It also returns
;the altered row.
(defun traverse-row-replace (row c v)
    (COND ((= c 0) (CONS v (traverse-row-replace (CDR row) (- c 1) v)))
          ((= (LENGTH row) 1) (APPEND row NIL))
          ((NOT (= c 0)) (CONS (CAR row) (traverse-row-replace (CDR row) (- c 1) v)))
          ;(t (APPEND (CAR row) (traverse-row-replace (CDR row) (- c 1) v)))
     )
 )

;set-square takes a state, a coordinate (c,r) and a value and returns a state
;with its value altered at (c,r). 
;First, I have to check if the coordinates (c,r) are out of bounds. If they are,
;return the original state unaltered.
;If r = 0, then traverse that row since the element to be changed is in that row.
;Next if the state only has 1 row left, then return that row.
;I had to write this condition because it made formatting easier.
;If r is not 0, then we are not at the correct row, and we continue iterating downwards.
(defun set-square (s c r v)
    (COND ((<= (getRows s) r) s)
          ((<= (getCols s) c) s)
          ((= r 0) (CONS (traverse-row-replace (CAR s) c v) (set-square (CDR s) c (- r 1) v)))
          ((= (LENGTH s) 1) s)
          ((NOT (= r 0)) (CONS (CAR s) (set-square (CDR s) c (- r 1) v)))
     )
 )
 
;This function checks if the keeper is blocked by a wall in any direction. 
;It will return true if keeper is blocked. Otherwise it returns NIL.
;First, I check for the direction the keeper is going.
;Then, I check if there is a wall in the way of where the keeper is walking.
;If there is, then return T, else NIL.
 (defun keeper-blocked-by-wall (s d)
    (COND ((= d 0) 
           (COND ((= (get-square s (FIRST (getKeeperPosition s 0)) (- (SECOND (getKeeperPosition s 0)) 1)) 1) T)
                 (t NIL)
            )
           )
          ((= d 1) 
           (COND ((= (get-square s (FIRST (getKeeperPosition s 0)) (+ (SECOND (getKeeperPosition s 0)) 1)) 1) T)
                 (t NIL)
            )
           )
          ((= d 2) 
           (COND ((= (get-square s (- (FIRST (getKeeperPosition s 0)) 1) (SECOND (getKeeperPosition s 0))) 1) T)
                 (t NIL)
            )
           )
          ((= d 3) 
           (COND ((= (get-square s (+ (FIRST (getKeeperPosition s 0)) 1) (SECOND (getKeeperPosition s 0))) 1) T)
                 (t NIL)
            )
           )
     )
 )
  
;This function checks if a box can be pushed.
;If a box is blocked by a wall or another box, then return T.
;First, I check which direction the box is being pushed.
;Then, I check if a wall (1), box (2) or goal+box (5) is in the way.
;If one of those are in the way, then return T.
;Otherwise, the box is not impeded and can be pushed.
(defun box-blocked (s d c r)
    (COND ((= d 0) 
               (COND ((= (get-square s c (- r 1)) 1) T)
                     ((= (get-square s c (- r 1)) 2) T)
                     ((= (get-square s c (- r 1)) 5) T)
                     (t NIL)
                )
           )
          ((= d 1) 
               (COND ((= (get-square s c (+ r 1)) 1) T)
                     ((= (get-square s c (+ r 1)) 2) T)
                     ((= (get-square s c (+ r 1)) 5) T)
                     (t NIL)
                )
           )
          ((= d 2) 
               (COND ((= (get-square s (- c 1) r) 1) T)
                     ((= (get-square s (- c 1) r) 2) T)
                     ((= (get-square s (- c 1) r) 5) T)
                     (t NIL)
                )
           )
          ((= d 3) 
               (COND ((= (get-square s (+ c 1) r) 1) T)
                     ((= (get-square s (+ c 1) r) 2) T)
                     ((= (get-square s (+ c 1) r) 5) T)
                     (t NIL)
                )
           )
    )
)

;This function deals with the change in the old square left by the keeper.
;If the keeper was on a square with int 3, then the square he leaves behind
;should become an empty space (0). 
;If it was originally a goal+keeper (6), then make it just a goal (4).
(defun vacated-square (s c r)
    (COND ((= (get-square s c r) 3) (set-square s c r 0))
          ((= (get-square s c r) 6) (set-square s c r 4))
          (t s)
     )
 )

 ;This function moves a box and alters the state accordingly.
 ;First, I checked if the box is blocked.
 ;Then, I check what direction the box is being pushed.
 ;Next, check if a square is a goal+box (5) or a box (2).
 ;If it is a goal+box, if the box is being pushed into another goal (4),
 ;make that square a goal+box and make the old square a keeper+goal (6),
 ;since the keeper is walking onto the goal. 
 ;If original square is a box, then change the square to just a keeper (3),
 ;since the keeper is not walking onto a goal.
(defun move-box (s d c r)
    (COND ((box-blocked s d c r) NIL)
          (t 
               (COND ((= d 0)                      
                      (COND ((= (get-square s c r) 5) 
                                 (COND ((= (get-square s c (- r 1)) 4) (set-square (set-square s c (- r 1) 5) c r 6))
                                       (t (set-square (set-square s c (- r 1) 2) c r 6))
                                 )
                            )
                            ((= (get-square s c r) 2) 
                                 (COND ((= (get-square s c (- r 1)) 4) (set-square (set-square s c (- r 1) 5) c r 3))
                                     (t (set-square (set-square s c (- r 1) 2) c r 3))
                                  )
                             )
                       )
                     )
                    ((= d 1) 
                      (COND ((= (get-square s c r) 5) 
                                 (COND ((= (get-square s c (+ r 1)) 4) (set-square (set-square s c (+ r 1) 5) c r 6))
                                       (t (set-square (set-square s c (+ r 1) 2) c r 6))
                                  )
                            )
                            ((= (get-square s c r) 2) 
                                 (COND ((= (get-square s c (+ r 1)) 4) (set-square (set-square s c (+ r 1) 5) c r 3))
                                       (t (set-square (set-square s c (+ r 1) 2) c r 3))
                                  )
                             )
                       )
                     )
                     ((= d 2) 
                      (COND ((= (get-square s c r) 5) 
                                 (COND ((= (get-square s (- c 1) r) 4) (set-square (set-square s (- c 1) r 5) c r 6))
                                       (t (set-square (set-square s (- c 1) r 2) c r 6))
                                  )
                             )
                            ((= (get-square s c r) 2) 
                                 (COND ((= (get-square s (- c 1) r) 4) (set-square (set-square s (- c 1) r 5) c r 3))
                                       (t (set-square (set-square s (- c 1) r 2) c r 3))
                                  )
                             )
                       )
                     )
                     ((= d 3) 
                      (COND ((= (get-square s c r) 5) 
                                 (COND ((= (get-square s (+ c 1) r) 4) (set-square (set-square s (+ c 1) r 5) c r 6))
                                       (t (set-square (set-square s (+ c 1) r 2) c r 6))
                                  )
                             )
                            ((= (get-square s c r) 2) 
                                 (COND ((= (get-square s (+ c 1) r) 4) (set-square (set-square s (+ c 1) r 5) c r 3))
                                       (t (set-square (set-square s (+ c 1) r 2) c r 3))
                                  )
                             )
                       )
                     )
                )
           )
       )
  )

;This function outputs a valid state or a NIL, depending on the original state
;and direction entered.
;First, check if the keeper is blocked. If keeper is blocked, return NIL.
;If the keeper is walking into a goal (4), change it to a keeper+goal(6).
;If the keeper is walking into a white-space (0), change it to a keeper (3).
;If the keeper is pushing a box, then check if the box is blocked. 
;If it is, return NIL.
;If not, then move the box.
;Notice that vacated-square is called to change the keeper's original square.
 (defun try-move (s d x y)
    (COND ((= d 0)
               (COND ((keeper-blocked-by-wall s d) NIL)
                     ((= (get-square s x (- y 1)) 4) 
                      (set-square (vacated-square s x y) x (- y 1) 6))
                     ((= (get-square s x (- y 1)) 0)
                      (set-square (vacated-square s x y) x (- y 1) 3))
                     ((OR (= (get-square s x (- y 1)) 2) (= (get-square s x (- y 1)) 5))
                          (COND ((equal (move-box s d x (- y 1)) NIL) NIL)
                                (t (move-box (vacated-square s x y) d x (- y 1)))
                          )
                      )
                )     
           )                         
          ((= d 1)
               (COND ((keeper-blocked-by-wall s d) NIL)
                     ((= (get-square s x (+ y 1)) 4) 
                      (set-square (vacated-square s x y) x (+ y 1) 6))
                     ((= (get-square s x (+ y 1)) 0)
                      (set-square (vacated-square s x y) x (+ y 1) 3))
                     ((OR (= (get-square s x (+ y 1)) 2) (= (get-square s x (+ y 1)) 5))
                          (COND ((equal (move-box s d x (+ y 1)) NIL) NIL)
                                (t (move-box (vacated-square s x y) d x (+ y 1)))
                          )
                      )
                )     
           )         
          ((= d 2)
               (COND ((keeper-blocked-by-wall s d) NIL)
                     ((= (get-square s (- x 1) y) 4) 
                      (set-square (vacated-square s x y) (- x 1) y 6))
                     ((= (get-square s (- x 1) y) 0)
                      (set-square (vacated-square s x y) (- x 1) y 3))
                     ((OR (= (get-square s (- x 1) y) 2) (= (get-square s (- x 1) y) 5))
                          (COND ((equal (move-box s d (- x 1) y) NIL) NIL)
                                (t (move-box (vacated-square s x y) d (- x 1) y))
                          )
                      )
                )     
           )
          ((= d 3)
               (COND ((keeper-blocked-by-wall s d) NIL)
                     ((= (get-square s (+ x 1) y) 4) 
                      (set-square (vacated-square s x y) (+ x 1) y 6))
                     ((= (get-square s (+ x 1) y) 0)
                      (set-square (vacated-square s x y) (+ x 1) y 3))
                     ((OR (= (get-square s (+ x 1) y) 2) (= (get-square s (+ x 1) y) 5))
                          (COND ((equal (move-box s d (+ x 1) y) NIL) NIL)
                                (t (move-box (vacated-square s x y) d (+ x 1) y))
                          )
                      )
                )     
           )
     )
)

;This function tries all 4 directions and outputs only the valid states.
;I decided to do the following configuration:
;0 -> up
;1 -> down
;2 -> left
;3 -> right
;I also passed in x and y, so I didn't have to call getKeeperPosition 100 times. 
;DISCLAIMER: I could not get this to work with states such as p4 and p7 where
;the states are not confined all around by walls. Testing the sokoban function 
;or next-states function with those types of states causes Stack Overflow because
;I am a bad programmer. For all other states, these functions work fine.
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (LIST (try-move s 0 x y) (try-move s 1 x y) (try-move s 2 x y) (try-move s 3 x y))
      )
	 )
    (cleanUpList result);end
   );end let
  );

;HEADER COMMENT for h0: Admissible because you cannot solve a problem in negative number of steps.
;Do I really have to comment what this function does?
(defun h0 (s)
	0
  )

;HEADER COMMENT for h1: Admissible because each misplaced box requires at least one step to 
;get pushed into a goal.

;This function merely calls the number of misplaced boxes (2) in the state.
(defun h1 (s)
	(COND ((NULL s) 0)
          ((ATOM s) 
               (COND ((isBox s) 1)
                     (t 0)
               )
          )
          (t (+ (h1 (CAR s)) (h1 (CDR s))))
     )
 )

;This function calculates the distance from one object to another.
;For example, say you have a box at (1,1) and a goal at (2,2).
;This function takes in a list of coordinates in the form of (1,1,2,2).
;It will then do calculations on the list.
;It basically takes the absolute value of the difference between 
;the respective x-coordinates and y-coordinates and adds them up
;to get the Manhattan distance. 
(defun calcDistance (objList)
    (COND ((AND (< (- (first objList) (third objList)) 0) (< (- (second objList) (fourth objList)) 0)) 
           (+ (* (- (first objList) (third objList)) -1) (* (- (second objList) (fourth objList)) -1)))
          ((< (- (first objList) (third objList)) 0) 
           (+ (* (- (first objList) (third objList)) -1) (- (second objList) (fourth objList))))
          ((< (- (second objList) (fourth objList)) 0) 
           (+ (- (first objList) (third objList)) (* (- (second objList) (fourth objList)) -1)))
          (t (+ (- (first objList) (third objList)) (- (second objList) (fourth objList))))
     )
 )
 
 ;This function finds the box that is closest to the keeper. 
 ;It returns values in the form of (distance (r,c)) where 
 ;distance is an int and (r,c) are the coordinates of the closest box.
 ;If there is only one box, then return the distance and that box.
 ;If there are two boxes, then return the closer box.
 ;If there are more than 2 boxes, then compare the first box in the list
 ;to the result of the recursion of the function.
 (defun closestBoxToKeeper (s boxes)
    (COND ((= (LENGTH boxes) 1) (LIST (calcDistance (APPEND (FIRST boxes) (getKeeperPosition s 0))) (FIRST boxes)))
          ((= (LENGTH boxes) 2)  
               (COND ((<= (calcDistance (APPEND (FIRST boxes) (getKeeperPosition s 0))) (calcDistance (APPEND (SECOND boxes) (getKeeperPosition s 0)))) 
                      (LIST (calcDistance (APPEND (FIRST boxes) (getKeeperPosition s 0))) (FIRST boxes))
                     )
                     (t (LIST (calcDistance (APPEND (SECOND boxes) (getKeeperPosition s 0))) (SECOND boxes))
                    )
               )
           )
               
              (t 
                   (COND ((<= (calcDistance (APPEND (FIRST boxes) (getKeeperPosition s 0))) (FIRST (closestBoxToKeeper s (REST boxes)))) 
                          (LIST (calcDistance (APPEND (FIRST boxes) (getKeeperPosition s 0))) (FIRST boxes))
                          )
                    
                     (t (closestBoxToKeeper s (REST boxes)))
                    )
               )
       )          
 )

;This function finds all the misplaced boxes in a state. 
;It returns a list of coordinates of such boxes.
;If the state is NULL, then return NIL.
;If s is one element, check that element. If it is a box, return its coordinates.
;If s is a list or list of lists, then use recursion to iterate through the
;rows and columns. 
(defun findBoxes (s r c)
    (COND ((NULL s) NIL)
          ((ATOM s)
               (COND ((isBox s) (LIST (CONS (- c 1) (CONS r NIL))))
                     (t NIL)
                )
           )
          (t ;(APPEND (findBoxes (CAR s) (+ r 1) c) (findBoxes (CDR s) r (+ c 1)))
               (COND ((= c 0) (APPEND (findBoxes (CAR s) r (+ c 1)) (findBoxes (CDR s) (+ r 1) c)))
                     (t (APPEND (findBoxes (CAR s) r c) (findBoxes (CDR s) r (+ c 1))))
                )                                     
           )
     )
) 

;This function does the same thing as findBoxes except it finds goals.
(defun findGoals (s r c)
    (COND ((NULL s) NIL)
          ((ATOM s)
               (COND ((isStar s) (LIST (CONS (- c 1) (CONS r NIL))))
                     (t NIL)
                )
           )
          (t ;(APPEND (findBoxes (CAR s) (+ r 1) c) (findBoxes (CDR s) r (+ c 1)))
               (COND ((= c 0) (APPEND (findGoals (CAR s) r (+ c 1)) (findGoals (CDR s) (+ r 1) c)))
                     (t (APPEND (findGoals (CAR s) r c) (findGoals (CDR s) r (+ c 1))))
                )                                     
           )
     )
)

;This function calculates the distance between a box and its closest goal.
;If there is only one goal, then return that distance.
;If there are more goals, then use recursion to iterate and find the closest goal.
(defun calcDistanceGoalToBox (box goals)
    (COND ((= (LENGTH goals) 1) (calcDistance (APPEND box (APPEND (FIRST goals) (SECOND goals)))))
          (t 
               (COND ((<= (calcDistance (APPEND box (FIRST goals))) (calcDistanceGoalToBox box (REST goals))) 
                      (calcDistance (APPEND box (FIRST goals)))
                      )
                     (t (calcDistanceGoalToBox box (REST goals)))
                )
           )
      )
 )

;This function adds up the distance from each box to its closest goal.
;If there are no boxes, return 0.
;If there is one box, return its distance to the closest goal.
;Otherwise, use recursion to add up the distances.
(defun addDistanceGoalsToBoxes (boxes goals)
    (COND ((= (LENGTH boxes) 0) 0)
          ((= (LENGTH boxes) 1) (calcDistanceGoalToBox (APPEND (FIRST boxes) (REST boxes)) goals))
          (t (+ (calcDistanceGoalToBox (FIRST boxes) goals) (addDistanceGoalsToBoxes (REST boxes) goals)))
    )
)

;HEADER COMMENT for h<hideUID>: This heuristic is admissible because it never 
;overestimates the steps necessary to solve a problem. It adds up the distance 
;from the keeper to the closest box. Then, it adds the distances from each box
;to its closest goal. I did not try to match each box with a unique goal because
;that can result in overestimation and an inadmissible heuristic.

;First, it checks if the state exists. If not, then it's in a goal state.
;If there are no boxes, then it's in a goal state. 
;I had to add the next two conditions because the interpreter would not 
;stop complaining about the possibility of a NIL being passed in.
;Finally, add up the distances as described in the HEADER COMMENT.
(defun h<hideUID> (s)
	(COND ((NULL s) 0)
          ((= (LENGTH (findBoxes s 0 0)) 0) 0)
          ((NULL (findBoxes s 0 0)) 0)
          ((NULL (findGoals s 0 0)) 0)
          (t (+ (FIRST (closestBoxToKeeper s (findBoxes s 0 0))) (addDistanceGoalsToBoxes (findBoxes s 0 0) (findGoals s 0 0))))
    ) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
