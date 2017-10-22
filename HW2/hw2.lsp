; These functions implement a depth-first iterative-deepening solver for the
; missionary-cannibal problem. In this problem, three missionaries and three
; cannibals are trying to go from the east side of a river to the west side.
; They have a single boat that can carry two people at a time from one side of
; the river to the other. There must be at least one person in the boat to cross
; the river. There can never be more cannibals on one side of the river than
; missionaries. If there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list (MISSIONARIES
; CANNIBALS SIDE). SIDE represents which side the boat is currently on, and is T
; if it is on the east side and NIL if on the west side. MISSIONARIES and
; CANNIBALS represent the number of missionaries and cannibals on the same side
; as the boat. Thus, the initial state for this problem is (3 3 T) (three
; missionaries, three cannibals, and the boat are all on the east side of the
; river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function ID-DFS, which is called
; with the initial state to search from and the depth up to which depth-first
; search will be performed. It returns the complete path from the initial state
; to the goal state: this path is a list of intermediate problem states. The
; first element of the path is the initial state and the last element is the
; goal state. Each intermediate state is the state that results from applying
; the appropriate operator to the preceding state.

; To solve the original problem, one would call (ID-DFS '(3 3 T) 0). 

; Examples of calls to some of the helper functions can be found after the code.


; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.

;It checks if the three elements of the list correspond with their 
;counterpart elements in the goal state.
;If yes, return True, else False.
(defun final-state (s)
	(COND ((AND (= (first s) 3) (= (second s) 3) (equal (third s) NIL)) T)
		  (t NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.

;First, I check if performing the action results in a negative number of 
;missionaries or cannibals.
;Next, I check if performing the action results in cannibals outnumbering 
;missionaries on either side of the river.
;If none of those conditions are true, then the action is valid and 
;it produces a valid state.
(defun next-state (s m c)
	(COND ((< (- (first s) m) 0) NIL)
		  ((< (- (second s) c) 0) NIL)
		  ;((AND (< (- (first s) m) (- (second s) c)) (> (- (first s) m) 0)) NIL)
          ;((AND (> (- (first s) m) (- (second s) c)) (> (- (second s) c) 0)) NIL)
          ((AND (< (- (first s) m) (- (second s) c)) (NOT (= (- (first s) m) 0))) NIL)
          ((AND (< (+ (- 3 (first s)) m) (+ (- 3 (second s)) c)) (NOT (= (+ (- 3 (first s)) m) 0))) NIL)
          (t (LIST (LIST (+ (- 3 (first s)) m) (+ (- 3 (second s)) c) (not (third s)))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.

;This function merely appends all the valid states into one single list.
;The next-state function does all the state validation.
(defun succ-fn (s)
    (APPEND (next-state s 1 0) (next-state s 0 1) (next-state s 1 1) (next-state s 2 0) (next-state s 0 2))
)

; MULT-DFS is a helper function for SINGLE-DFS. It takes three arguments: the
; path from the initial state to the current state (PATH), the legal successor
; states to the last state on PATH (STATES), and the depth (DEPTH). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a single depth-first iteration to the given depth on
; each element of STATES in turn. If any of those searches reaches the final
; state, MULT-DFS returns the complete path from the initial state to the goal
; state. Otherwise, it returns NIL.

;First, I check if the state is valid. If yes, return it.
;Next if there are more states, then traverse those too.
;Otherwise, return nothing.
(defun mult-dfs (states path depth)
    (COND ((single-dfs (CAR states) path depth) (single-dfs (CAR states) path depth))
          ((NOT (NULL (CDR states))) (mult-dfs (CDR states) path depth))
          (t NIL)))

; SINGLE-DFS does a single depth-first iteration to the given depth. It takes
; three arguments: a state (S), the path from the initial state to S (PATH), and
; the depth (DEPTH). If S is the initial state in our search, PATH should be
; NIL. It performs a depth-first search starting at the given state. It returns
; the path from the initial state to the goal state, if any, or NIL otherwise.

;First, check if depth is not negative. If it is, then return.
;If you reach the goal state, then add it to the path.
;Otherwise, recurse through the next level of the tree.
(defun single-dfs (s path depth)
	(COND ((< depth 0) NIL)
          ((final-state s) (APPEND path (LIST s)))   
		  (t (mult-dfs (succ-fn s) (APPEND path (LIST s)) (- depth 1)))))

; ID-DFS is the top-level function. It takes two arguments: an initial state (S)
; and a search depth (DEPTH). ID-DFS performs a series of depth-first
; iterations, starting from the given depth until a solution is found. It
; returns the path from the initial state to the goal state. The very first call
; to ID-DFS should use depth = 0.

;If the current depth returns nothing, then go to the next level of the tree.
;Otherwise, traverse the current depth.
(defun id-dfs (s depth)
    (COND ((EQUAL (single-dfs s NIL depth) NIL) (id-dfs s (+ depth 1)))
          (t (single-dfs s NIL depth))))

;These functions are for questions 1 and 2.

;First, check if the tree is null. If it is return nothing.
;If the tree is an atom, return that atom in a list.
;If the tree is length 1, return a traversal of that thing.
;The reason I have two cases is because (7) and 7 are different.
;Otherwise, traverse the tree and append the result.
(defun DFS (tree)
	(COND ((NULL tree) NIL)
          ((ATOM tree) (CONS tree NIL))
          ((= (LENGTH tree) 1) (DFS (first tree)))
          (t (APPEND (DFS (first tree)) (DFS (rest tree))))))

;Check if the depth is not negative. If it is, return nothing.
;If the tree is empty, return nothing.
;If the tree is an atom, return that atom in a list.
;If the tree is length 1, return a traversal of that thing.
;Otherwise, traverse the tree and append the result.
;This is just a modified DFS function with a depth argument.		  
(defun DFID_helper (tree depth)
	(COND ((< depth 0) NIL)
          ((NULL tree) NIL)
          ((ATOM tree) (CONS tree NIL))
          ((= (LENGTH tree) 1) (DFID_helper (first tree) (- depth 1)))
          (t (APPEND (DFID_helper (first tree) (- depth 1)) (DFID_helper (rest tree) depth)))))

;Check if it has surpassed the max depth. If yes, return nothing.
;If the tree is empty, return nothing.
;Else, append the traversal of the current depth with the 
;traversal of the rest of the tree.
(defun DFID (tree depth)
    (COND ((< depth 0) NIL)
          ((NULL tree) NIL)
          (t (APPEND (DFID tree (- depth 1)) (DFID_helper tree depth)))))


