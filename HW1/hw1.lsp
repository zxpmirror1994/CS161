; PAD takes a single argument that represents the index of the Padovan number that we intend find. It returns the Nth Padovan number if the argument is N.
(defun PAD (N) 
(cond ((< N 0) nil) ; Base case #1
      ((= N 0) 1)   ; Base case #2
      ((= N 1) 1)   ; Base case #3
      ((= N 2) 1)   ; Base case #4
      (t (+ (PAD (- N 2)) (PAD (- N 3)))))) ; If not a base case, then call recursively on PAD

; SUMS takes a single numeric argument N, and returns the number of additions required by PAD function to compute the Nth Padovan number.
(defun SUMS (N)
(cond ((< N 0) nil) ; Base case #1
      ((= N 0) 0)   ; Base case #2
      ((= N 1) 0)   ; Base case #3
      ((= N 2) 0)   ; Base case #4
      (t (+ 1 (SUMS (- N 2)) (SUMS (- N 3)))))) ; If not a base case, then call recursively on SUMS 

; ANON takes a single argument TREE, which can be an atom, a list or nil, and returns an anonymized tree with the same structure as TREE, but where all symbols and numbers in the tree are replaced by a question mark.
(defun ANON (TREE)
(cond ((null TREE) nil) ; Base case #1
      ((atom TREE) '?)  ; Base case #2
      (t (cons (ANON (car TREE)) (ANON (cdr TREE)))))) ; If not a base case, then call recursively on ANON to handle the first entry and the rest entries of the tree.

