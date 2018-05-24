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

; ----------------------------------- Helper functions ------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Game implementation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; getRow (s), s is a state, i.e. a list of lists
; This function returns the number of rows in certain state by checking how many lists s contains.
; The value returned can be used as a boundary checking parameter.
(defun getRow (s)
(cond 	((null (car s)) 0)
	(t (+ 1 (getRow (cdr s))))))	

; getColumn (row), row is any element in a state, i.e. a list of atoms 
; This function returns the number of columns in certain state by checking how many atoms a row contains.
; A row can be any list in s. The value returned can be used as a boundary checking parameter.
(defun getColumn (row)
(cond	((null (car row)) 0)
	(t (+ 1 (getColumn (cdr row))))))

; getValue (s coor), s is the state, coor is the position whose value we want to retrieve
; This function returns the value on the position "coor" in the state "s".
(defun getValue (s coor)
(cond 	((null s) NIL)
	(t (car (nthcdr (car coor) (car (nthcdr (car (cdr coor)) s)))))))

; getKeeperValue (s), s is a state
; This function tells either the keeper is standing on a blank or the keeper is standing on a goal.
(defun getKeeperValue (s)
(getValue s (getKeeperPosition s 0)))
;(car (nthcdr (car (getKeeperPosition s 0)) (car (nthcdr (car (cdr (getKeeperPosition s 0))) s)))))

; getKeeperNextValue (s dir), s is a state, dir is the direction that the keeper moves on
; This function returns the value (from 0 to 6) of one of four positions next to the keeper. The argument
; "dir" specifies which of four positions to be returned.
(defun getKeeperNextValue (s dir)
(cond  	((equal dir 'UP) (cond ((= 0 (car (cdr (getKeeperPosition s 0)))) NIL)
			    	(t (car (nthcdr (car (getKeeperPosition s 0)) (car (nthcdr (- (car (cdr (getKeeperPosition s 0))) 1) s)))))))
	((equal dir 'DOWN) (cond ((= (- (getRow s) 1) (car (cdr (getKeeperPosition s 0)))) NIL)
				(t (car (nthcdr (car (getKeeperPosition s 0)) (car (nthcdr (+ (car (cdr (getKeeperPosition s 0))) 1) s)))))))
	((equal dir 'LEFT) (cond ((= 0 (car (getKeeperPosition s 0))) NIL)
				(t (car (nthcdr (- (car (getKeeperPosition s 0)) 1) (car (nthcdr (car (cdr (getKeeperPosition s 0))) s)))))))
	((equal dir 'RIGHT) (cond ((= (- (getColumn (car s)) 1) (car (getKeeperPosition s 0))) NIL)
				(t (car (nthcdr (+ (car (getKeeperPosition s 0)) 1) (car (nthcdr (car (cdr (getKeeperPosition s 0))) s)))))))
	(t NIL)))

; getKeeperNextCoor (s dir), s is a state, dir is the direction that the keeper moves on
; This function returns the coordinate (c,r) of one of four positions next to the keeper. The argument
; "dir" specifies which of four positions to be returned.
(defun getKeeperNextCoor (s dir)
(cond 	((null  (getKeeperNextValue s dir)) NIL)
	(t (cond ((equal dir 'UP) (list (car (getKeeperPosition s 0)) (- (car (cdr (getKeeperPosition s 0))) 1)))
		 ((equal dir 'DOWN) (list (car (getKeeperPosition s 0)) (+ (car (cdr (getKeeperPosition s 0))) 1)))
		 ((equal dir 'LEFT) (list (- (car (getKeeperPosition s 0)) 1) (car (cdr (getKeeperPosition s 0)))))
		 ((equal dir 'RIGHT) (list (+ (car (getKeeperPosition s 0)) 1) (car (cdr (getKeeperPosition s 0)))))
		 (t NIL)))))

; getKeeperNextNextValue (s dir), s is a state, dir is the direction that the keeper moves on
; This function returns the value (from 0 to 6) of one of four positions that has exactly one block between it and the keeper.
; This function is useful because if the keeper pushes a box in one move, the box will be pushed onto another block whose
; value will be returned by this function. The argument "dir" specifies which of four positions to be returned.
(defun getKeeperNextNextValue (s dir)
(cond 	((null (getKeeperNextValue s dir)) NIL)
	(t (cond ((equal dir 'UP) (cond ((= 1 (car (cdr (getKeeperPosition s 0)))) NIL)
					(t (car (nthcdr (car (getKeeperPosition s 0)) (car (nthcdr (- (car (cdr (getKeeperPosition s 0))) 2) s)))))))
		 ((equal dir 'DOWN) (cond ((= (- (getRow s) 2) (car (cdr (getKeeperPosition s 0)))) NIL)
					(t (car (nthcdr (car (getKeeperPosition s 0)) (car (nthcdr (+ (car (cdr (getKeeperPosition s 0))) 2) s)))))))
		 ((equal dir 'LEFT) (cond ((= 1 (car (getKeeperPosition s 0))) NIL)
					(t (car (nthcdr (- (car (getKeeperPosition s 0)) 2) (car (nthcdr (car (cdr (getKeeperPosition s 0))) s)))))))
		 ((equal dir 'RIGHT) (cond ((= (- (getColumn (car s)) 2) (car (getKeeperPosition s 0))) NIL)
					(t (car (nthcdr (+ (car (getKeeperPosition s 0)) 2) (car (nthcdr (car (cdr (getKeeperPosition s 0))) s)))))))
		 (t NIL)))))

; getKeeperNextNextCoor (s dir), s is a state, dir is the direction that the keeper moves on
; This function returns the coordinate (c,r) of one of four positions that has exactly one block between it and the keeper.
; This function is useful because if the keeper pushes a box in one move, the box will be pushed onto another block whose
; value will be returned by this function. The argument "dir" specifies which of four positions to be returned.
(defun getKeeperNextNextCoor (s dir)
(cond 	((null  (getKeeperNextNextValue s dir)) NIL)
	(t (cond ((equal dir 'UP) (list (car (getKeeperPosition s 0)) (- (car (cdr (getKeeperPosition s 0))) 2)))
		 ((equal dir 'DOWN) (list (car (getKeeperPosition s 0)) (+ (car (cdr (getKeeperPosition s 0))) 2)))
		 ((equal dir 'LEFT) (list (- (car (getKeeperPosition s 0)) 2) (car (cdr (getKeeperPosition s 0)))))
		 ((equal dir 'RIGHT) (list (+ (car (getKeeperPosition s 0)) 2) (car (cdr (getKeeperPosition s 0)))))
		 (t NIL)))))

; changeState (s coor kind), s is the currend state, coor is the coordinate on the state that we want to change value, kind is the value we want to change
; This function alters the current value of position, which is determined by "coor" in the state "s", to the new value "kind". The return value of this
; function is the updated state.
(defun changeState (s coor kind)
(cond 	((null coor) NIL)
	(t (append
		(butlast s (- (getRow s) (car (cdr coor))))
		(list (append 
			(butlast (car (nthcdr (car (cdr coor)) s)) (- (getColumn (car s)) (car coor))) 
			(list kind)
			(nthcdr (+ (car coor) 1) (car (nthcdr (car (cdr coor)) s)))
		))
		(nthcdr (+ (car (cdr coor)) 1) s)))))

; pushBox (s dir), s is a state, dir is the direction that the keeper pushes the box
; This function indicates what happens if the keeper push a box in certain direction. This function is called only if the keeper knows that there is
; a box next to him/her. The box can only be pushed if there is a blank or a star next to it. The direction it get pushed should be exactly the same
; as the direction it is on the keeper. (For example, if there is a box above the keeper, the box can only be pushed up). The returned value is the
; state after the box is pushed or NIL is the box cannot be pushed or no box locate next to the keeper.
(defun pushBox (s dir)
(cond 	((null (getKeeperNextNextValue s dir)) NIL)	
	((isWall (getKeeperNextNextValue s dir)) NIL)	
	((isBlank (getKeeperNextNextValue s dir))
		(cond  	((and (isKeeperStar (getKeeperValue s)) (isBoxStar (getKeeperNextValue s dir))) 
				(changeState (changeState (changeState s (getKeeperNextNextCoor s dir) 2) (getKeeperNextCoor s dir) 6) (getKeeperPosition s 0) 4))
		      	((and (isKeeperStar (getKeeperValue s)) (isBox (getKeeperNextValue s dir)))
		      		(changeState (changeState (changeState s (getKeeperNextNextCoor s dir) 2) (getKeeperNextCoor s dir) 3) (getKeeperPosition s 0) 4))
			((and (isKeeper (getKeeperValue s)) (isBoxStar (getKeeperNextValue s dir)))
		      		(changeState (changeState (changeState s (getKeeperNextNextCoor s dir) 2) (getKeeperNextCoor s dir) 6) (getKeeperPosition s 0) 0))
			((and (isKeeper(getKeeperValue s)) (isBox (getKeeperNextValue s dir)))
		      		(changeState (changeState (changeState s (getKeeperNextNextCoor s dir) 2) (getKeeperNextCoor s dir) 3) (getKeeperPosition s 0) 0))))
	((isBox (getKeeperNextNextValue s dir)) NIL)	
	((isStar (getKeeperNextNextValue s dir))
		(cond  	((and (isKeeperStar (getKeeperValue s)) (isBoxStar (getKeeperNextValue s dir))) 
				(changeState (changeState (changeState s (getKeeperNextNextCoor s dir) 5) (getKeeperNextCoor s dir) 6) (getKeeperPosition s 0) 4))
		      	((and (isKeeperStar (getKeeperValue s)) (isBox (getKeeperNextValue s dir)))
		      		(changeState (changeState (changeState s (getKeeperNextNextCoor s dir) 5) (getKeeperNextCoor s dir) 3) (getKeeperPosition s 0) 4))
			((and (isKeeper (getKeeperValue s)) (isBoxStar (getKeeperNextValue s dir)))
		      		(changeState (changeState (changeState s (getKeeperNextNextCoor s dir) 5) (getKeeperNextCoor s dir) 6) (getKeeperPosition s 0) 0))
			((and (isKeeper(getKeeperValue s)) (isBox (getKeeperNextValue s dir)))
		      		(changeState (changeState (changeState s (getKeeperNextNextCoor s dir) 5) (getKeeperNextCoor s dir) 3) (getKeeperPosition s 0) 0))))
	((isBoxStar (getKeeperNextNextValue s dir)) NIL)))

; try-move (s dir), s is a state, dir is the direction that the keeper pushes the box
; This functions returns the state after one move of the keeper in one direction. "dir" can be any candidate of UP, DOWN, LEFT, RIGHT.
(defun try-move(s dir)
(cond	((null (getKeeperNextValue s dir)) NIL)
	((isWall (getKeeperNextValue s dir)) NIL)
	((isBlank (getKeeperNextValue s dir)) 
		(cond  	((isKeeperStar (getKeeperValue s))
		      		(changeState (changeState s (getKeeperNextCoor s dir) 3) (getKeeperPosition s 0) 4))
			(t	(changeState (changeState s (getKeeperNextCoor s dir) 3) (getKeeperPosition s 0) 0))))
	((isBox (getKeeperNextValue s dir)) (pushBox s dir))
	((isStar (getKeeperNextValue s dir)) 
		(cond  	((isKeeperStar (getKeeperValue s))
		      		(changeState (changeState s (getKeeperNextCoor s dir) 6) (getKeeperPosition s 0) 4))
			(t	(changeState (changeState s (getKeeperNextCoor s dir) 6) (getKeeperPosition s 0) 0))))
	((isBoxStar (getKeeperNextValue s dir)) (pushBox s dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Heuristic function related ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; getBoxColumn (r col)
; This function returns an arbitrary column that has a box (exclusing box on the goal). It is similar to getKeeperColumn (r col).
(defun getBoxColumn (r col)
(cond 	((null r) NIL)
	(t (cond ((isBox (car r)) col)
		 (t (getBoxColumn (cdr r) (+ col 1)))))))
	 
; getBoxPosition (s row)
; This function returns an arbitrary position that has a box (exclusing box on the goal). It is similar to getKeeperPosition (s row).
(defun getBoxPosition (s row)
(cond 	((null s) NIL)
	(t (cond ((getBoxColumn (car s) 0)
			(list (getBoxColumn (car s) 0) row))
		 (t (getBoxPosition (cdr s) (+ row 1)))))))

(defun getAllBoxPosition (s)
(cond 	((null s) NIL)
	(t (cons (getBoxPosition s 0) (getAllBoxPosition (changeState s (getBoxPosition s 0) 0)))))) 
; getAllBoxPosition (s)
; This function collects all box positions into a list. For certain state, we find an arbitrary box position. Changing its value to
; a blank results in a new state. Then, calling getBoxPosition (s row) will give us a new box position.
; Until there are no boxes in the state, make a list to accomodate all box positions. 
(defun getAllBoxPosition (s)
(cond 	((null s) NIL)
	(t (cons (getBoxPosition s 0) (getAllBoxPosition (changeState s (getBoxPosition s 0) 0)))))) 

; getStarColumn (r col)
; This function returns an arbitrary column that has a star (exclusing star with something on it). It is similar to getKeeperColumn (r col).
(defun getStarColumn (r col)
(cond 	((null r) NIL)
	(t (cond ((isStar (car r)) col)
		 (t (getStarColumn (cdr r) (+ col 1)))))))
	 
; getStarPosition (s row)
; This function returns an arbitrary position that has a star (exclusing star with something on it). It is similar to getKeeperPosition (r col).
(defun getStarPosition (s row)
(cond 	((null s) NIL)
	(t (cond ((getStarColumn (car s) 0)
			(list (getStarColumn (car s) 0) row))
		 (t (getStarPosition (cdr s) (+ row 1)))))))

; getAllStarPosition (s)
; This function collects all star positions into a list. For certain state, we find an arbitrary star position. Changing its value to
; a blank results in a new state. Then, calling getStarPosition (s row) will give us a new star position.
; Until there are no stars in the state, make a list to accomodate all star positions.
(defun getAllStarPosition (s)
(cond 	((null s) NIL)
	(t (cons (getStarPosition s 0) (getAllStarPosition (changeState s (getStarPosition s 0) 0)))))) 

; getDis (coor1 coor2), coor1 and coor2 are positions that we want to calculate the distance
(defun getDis (coor1 coor2)
(+ (abs (- (car coor1) (car coor2))) (abs (- (car (cdr coor1)) (car (cdr coor2))))))

; getClosestPair (lst coor), lst is a list containing several coordinates, coor is an individual coordinate
; This function find the smallest distance between "coor" and any element in "lst".
(defun getClosestPair (lst coor)
(cond 	((null lst) 0)
	((null (cdr lst)) (getDis (car lst) coor))
	((< (getDis (car lst) coor) (getDis (cadr lst) coor)) (getClosestPair (cons (car lst) (cddr lst)) coor))
	(t (getClosestPair (cdr lst) coor))))

; getClosestSum (lst1 lst2), lst1 and lst2 are two lists containing several coordinates
; This function sums up the smallest distances between each element in "lst2" and any element in "lst1".
; For example, if "lst1" is the list of all star positions (s1,s2,s3) and "lst2" is the list of all bos positions (b1,b2), this
; function will return getClosestPair (lst1 b1) + getClosestPair (lst1 b2). This is really useful for the heuristic function.
(defun getClosestSum (lst1 lst2)
(cond	((or (null lst1) (null lst2)) 0)
	((null (cdr lst2)) (getClosestPair lst1 (car lst2)))
	(t (+ (getClosestPair lst1 (car lst2)) (getClosestSum lst1 (cdr lst2))))))

; ----------------------------------- End of Helper functions ------------------------------------------


; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
(cond	((null (car s)) t)
	(t (cond ((= 0 (count box (car s))) (goal-test (cdr s)))
		(t nil)))));end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT)))
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s) 0)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;----------------------------------------------------------------------------------------------------------|
; Answer to question:                                                                                      |
; h1 is an admissible heuristic because it optimizes the steps that we need to take to win in              |
; Sokoban. Since for each step, the keeper is only allowed to move on box in one direction, it             |
; takes at least one step to put one misplaced box onto the goal. This means if we have to move "n"        |
; boxes to goals to win the game, it takes at least "n" steps, which is eqivalent to the value returned    |
; by h1. In more general situations, not each box is adjacent to one goal and the keeper is just beside    |
; the box, therefore it is probable to take more than one step to move any certain box to the goal.        |
; Hence, h1 sets up an optimistic lower bound for the number of steps to win in Sokoban. h1 is admissible. |
;----------------------------------------------------------------------------------------------------------|
(defun h1 (s)
(cond 	((null (car s)) 0)
	(t (+ (count box (car s)) (h1 (cdr s))))))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h004309991 (s)
(cond 	((null (car s)) 0)
	((= 0 (h1 s)) 0)
	(t (+ (getClosestSum (cleanUpList (getAllStarPosition s)) (cleanUpList (getAllBoxPosition s))) 
		(- (getDis (getKeeperPosition s 0) (getBoxPosition s 0)) 1)))))

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

;(2324,25)
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
;(470,21)
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
