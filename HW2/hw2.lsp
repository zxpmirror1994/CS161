;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; TODO: comment code
(defun BFS (FRINGE)
(cond ((null (car FRINGE)) nil)   ; If FRINGE contains nothing, return nil
      ((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))))   ; If the first element in FRINGE is an atom, visit this atom branch at first and append the result of BFS on the rest of FRINGE
      ((listp (car FRINGE)) (BFS (append (cdr FRINGE) (car FRINGE))))   ; If the first element in FRINGE is a list, append the element to the end of FRINGE since we won't visit it at once. Re-call BFS on the rearranged FRINGE 
      (t nil)))


;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.

(defun FINAL-STATE (S)
(cond ((equal S '(T T T T)) T)   ; If S equals '(T T T T}, we can tell it is the final state; otherwise, return NIL
      (t NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).

(defun NEXT-STATE (S A)
(cond ((equal (length S) 4)   ; If the current state is valid, we can find the next state    
       (cond ((equal A 'h)   ; Next move is homer 
              (cond ((or (and (equal (second S) (third S)) (equal (first S) (second S)))   ; If homer, dog and baby are on the same side, homer cannot leave alone
                         (and (equal (second S) (fourth S)) (equal (first S) (second S))))   ; If homer, baby and poison are on the same side, homer cannot leave alone 
                      NIL)
                    (t (list (list (not (first S)) (second S) (third S) (fourth S))))))   ; In other cases, homer can safely leave by himself
      
              ((and (equal A 'd) (equal (first S) (third S)))   ; Next move is homer+dog, homer and dog must be on the same side
               (cond ((and (equal (second S) (fourth S)) (equal (first S) (second S))) NIL)   ; If homer, baby and poison are on the same side, homer cannot leave with dog
                     (t (list (list (not (first S)) (second S) (not (third S)) (fourth S))))))   ; In other cases, homer can safely leave with dog 
      
              ((and (equal A 'b) (equal (first S) (second S)))   ; Next move is homer+baby, homer and baby must be on the same side
               (list (list (not (first S)) (not (second S)) (third S) (fourth S))))   ; In whatever situations, homer can safely leave with baby

              ((and (equal A 'p) (equal (first S) (fourth S)))   ; Next move is homer+poison, homer and poison must be on the same side
               (cond ((and (equal (second S) (third S)) (equal (first S) (second S))) NIL)   ; If homer, dog and baby are on the same side, homer cannot leave with poison
                     (t (list (list (not (first S)) (second S) (third S) (not (fourth S)))))))   ; In other cases, homer can safely leave with poison
      
              (t NIL)))
      (t NIL)))    ; If the current state is invalid, say it contains only 3 elements, return NIL

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.

(defun SUCC-FN (S)
(append (append (append 
      (cond ((null (NEXT-STATE S 'h)) NIL)
            (t (NEXT-STATE S 'h)))
      (cond ((null (NEXT-STATE S 'b)) NIL)
            (t (NEXT-STATE S 'b))))
      (cond ((null (NEXT-STATE S 'd)) NIL)
            (t (NEXT-STATE S 'd))))
      (cond ((null (NEXT-STATE S 'p)) NIL)
            (t (NEXT-STATE S 'p)))))   ; Append all successor states in the sequence of performing 'h, 'b, 'd, 'p

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.

(defun ON-PATH (S STATES)
(cond ((null STATES) NIL)   ; If STATES is nil, there is no way that S is in STATES
      ((null (car STATES)) NIL)   ; If STATES is (nil), there is also no way that S is in STATES
      (t (cond ((equal S (car STATES)) T)
               (t (ON-PATH S (cdr STATES)))))))   ; If S is not the first element of STATES, check if S is on the path comprised of rest of states

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.

(defun MULT-DFS (STATES PATH)
(cond ((null STATES) NIL)   ; If STATES is nil, return NIL
      ((null (DFS (car STATES) PATH)) (MULT-DFS (cdr STATES) PATH))   ; If DFS does not find the goal state by tracking down the first successor state, try MULT-DFS on the rest states
      (t (DFS (car STATES) PATH))))   ;If DFS does find the goal state, return the PATH that DFS should return

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.

(defun DFS (S PATH)
(cond ((FINAL-STATE S) (append PATH (list S)))   ; If S is already the final state, append it to the existing PATH
      ((ON-PATH S PATH) NIL)   ; If S is already been visited through this PATH, we do not have to visit it again, so return NIL
      (t (MULT-DFS (SUCC-FN S) (append PATH (list S))))))   ; If above situations do not apply, call MULT-DFS on all successors of S to see what will happen

