(defun add-row (str)
  "Takes a string (row of the input grid containing numbers) and outputs list with corresponding values.
  If value is not present will substitute -1 for it.
  Eg. for string ' 3 3   2 ' it will output list (3 3 -1 2)"
  (setf row '())
  (setq len (length str))
  (loop for i from 0 to (1- len)
        do (cond ((oddp i)
                  (setf row (append row (list (or (parse-integer str :start i :end (+ i 1) :junk-allowed t) -1))))
                  )
                 )
        )
  (return-from add-row row)
  )

(defun init-grid ()
  (setf grid '())
  (loop for i from 1 to num-rows
        do (progn
             (setf x '())
             (loop for j from 1 to num-cols
                   do (setf x (cons (list :right NIL :left NIL :top NIL :down NIL :degree 0) x))
                   )
             (setf grid (cons x grid))
             (makunbound 'x)
             )
        )
  )

(defun display-grid ()
  (format t "~%   ")
  (loop for k from 1 to (- num-cols 1)
        do (format t " ~A" k)
        )
  (format t " ~%~%")
  (loop for i from 0 to (- num-rows 2)
        do (progn (setf up-line "")
                  (setf b-line "")
                  (loop for j from 0 to (- num-cols 2)
                        do (progn (setf up-line (cond (
                                                       (null (getf (nth j (nth i grid)) :right))
                                                       (concatenate 'string up-line "+ ")
                                                       )
                                                      (T (concatenate 'string up-line "+-"))
                                                      ))
                                  (setf bchar (cond ((equal (nth j (nth i board)) -1) " ") (T (write-to-string (nth j (nth i board)))))) 
                                  (setf b-line (cond ((equal (getf (nth j (nth i grid)) :down) NIL) (concatenate 'string b-line " " bchar))
                                                     (T (concatenate 'string b-line "|" bchar))
                                                     ))))
                  (cond ((not (null (getf (nth (- num-cols 1) (nth i grid)) :down))) (setf b-line (concatenate 'string b-line "|"))))
                  (format t "   ~A+~%" up-line)
                  (format t "~A  ~A~%" (+ i 1)  b-line)
                  ))
  (format t "   ")
  (loop for i from 0 to (- num-cols 2)
        do (progn 
             (setf up-line "")
             (cond (
                    (null (getf (nth i (nth (- num-rows 1) grid)) :right))
                    (format t "~A" "+ ")
                    )
                   (T (format t "~A" "+-"))
                   )
             )
        )
  (format t "~A~%" "+")
  (format t "~%")
  )

(defun space-split (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\Space string :start start)
        collecting (subseq string start finish)
        until (null finish))
  )

(defun construct-edge (x y direc p q)
  ; (setf lnc (+ lnc 1))
  (setf (getf (nth y (nth x grid)) :degree) (+ (getf (nth y (nth x grid)) :degree) 1))
  (setf (getf (nth q (nth p grid)) :degree) (+ (getf (nth q (nth p grid)) :degree) 1))
  (cond (
         (equal direc "D")
         (progn
           (setf (getf (nth y (nth x grid)) :down) (list p q))
           (setf (getf (nth q (nth p grid)) :top) (list x y))
           )
         )
        (
         (equal direc "T")
         (progn
           (setf (getf (nth y (nth x grid)) :top) (list p q))
           (setf (getf (nth q (nth p grid)) :down) (list x y))
           )
         )
        (
         (equal direc "L")
         (progn
           (setf (getf (nth y (nth x grid)) :left) (list p q))
           (setf (getf (nth q (nth p grid)) :right) (list x y))
           )
         )
        (
         (equal direc "R")
         (progn
           (setf (getf (nth y (nth x grid)) :right) (list p q))
           (setf (getf (nth q (nth p grid)) :left) (list x y))
           )
         )
        )
  ; (display-grid)
  )

(defun remove-edge (x y direc p q)
  ; (format t "~% removing ~A ~A ~A ~A ~A ~%" x y direc p q)
  ; (setf lnc (- lnc 1))
  (setf (getf (nth y (nth x grid)) :degree) (- (getf (nth y (nth x grid)) :degree) 1))
  (setf (getf (nth q (nth p grid)) :degree) (- (getf (nth q (nth p grid)) :degree) 1)) 
  (cond (
         (equal direc "D")
         (progn
           (setf (getf (nth y (nth x grid)) :down) NIL)
           (setf (getf (nth q (nth p grid)) :top) NIL)
           )
         )
        (
         (equal direc "T")
         (progn
           (setf (getf (nth y (nth x grid)) :top) NIL)
           (setf (getf (nth q (nth p grid)) :down) NIL)
           )
         )
        (
         (equal direc "L")
         (progn
           (setf (getf (nth y (nth x grid)) :left) NIL)
           (setf (getf (nth q (nth p grid)) :right) NIL)
           )
         )
        (
         (equal direc "R")
         (progn
           (setf (getf (nth y (nth x grid)) :right) NIL)
           (setf (getf (nth q (nth p grid)) :left) NIL)
           )
         )
        )
  ; (display-grid)
  ; (print grid)
  ; (read-line t "Waiting ")
  )
  
  
(defun draw-line (x y direc)
  (cond (
         (equal direc "D")
         (cond (
                (null (getf (nth (- y 1) (nth x grid)) :right))
                (progn
                  (setf lnc (+ lnc 1))
                  (setf (getf (nth (- y 1) (nth x grid)) :right) (list x y))
                  (setf (getf (nth (- y 1) (nth x grid)) :degree) (+ (getf (nth (- y 1) (nth x grid)) :degree) 1))
                  (setf (getf (nth y (nth x grid)) :left) (list x (- y 1)))
                  (setf (getf (nth y (nth x grid)) :degree) (+ (getf (nth y (nth x grid)) :degree) 1))
                  )
                )
               (
                (not (null (getf (nth (- y 1) (nth x grid)) :right)))
                (progn
                  (setf lnc (- lnc 1))
                  (setf (getf (nth (- y 1) (nth x grid)) :right) NIL)
                  (setf (getf (nth (- y 1) (nth x grid)) :degree) (- (getf (nth (- y 1) (nth x grid)) :degree) 1))
                  (setf (getf (nth y (nth x grid)) :left) NIL)
                  (setf (getf (nth y (nth x grid)) :degree) (- (getf (nth y (nth x grid)) :degree) 1))
                  )
                )
               )
         
         )
        (
         
         (equal direc "T")
         (cond (
                (null (getf (nth (- y 1) (nth (- x 1) grid)) :right))
                (progn
                  (setf lnc (+ lnc 1))
                  (setf (getf (nth (- y 1) (nth (- x 1) grid)) :right) (list (- x 1) y))
                  (setf (getf (nth (- y 1) (nth (- x 1) grid)) :degree) (+ (getf (nth (- y 1) (nth (- x 1) grid)) :degree) 1))
                  (setf (getf (nth y (nth (- x 1) grid)) :left) (list (- x 1) (- y 1)))
                  (setf (getf (nth y (nth (- x 1) grid)) :degree) (+ (getf (nth y (nth (- x 1) grid)) :degree) 1))
                  )
                )
               (
                T
                (progn
                  (setf lnc (- lnc 1))
                  (setf (getf (nth (- y 1) (nth x grid)) :right) NIL)
                  (setf (getf (nth (- y 1) (nth x grid)) :degree) (- (getf (nth (- y 1) (nth x grid)) :degree) 1))
                  (setf (getf (nth y (nth x grid)) :left) NIL)
                  (setf (getf (nth y (nth x grid)) :degree) (- (getf (nth y (nth x grid)) :degree) 1))
                  )
                )
               )
         
         )
        (
         (equal direc "L")
         (cond (
                (null (getf (nth (- y 1) (nth (- x 1) grid)) :down))
                (progn
                  (setf lnc (+ lnc 1))
                  (setf (getf (nth (- y 1) (nth (- x 1) grid)) :down) (list x (- y 1)))
                  (setf (getf (nth (- y 1) (nth (- x 1) grid)) :degree) (+ (getf (nth (- y 1) (nth (- x 1) grid)) :degree) 1))
                  (setf (getf (nth (- y 1) (nth x grid)) :top) (list (- x 1) (- y 1)))
                  (setf (getf (nth (- y 1) (nth x grid)) :degree) (+ (getf (nth (- y 1) (nth x grid)) :degree) 1))
                  )
                )
               (
                T
                (progn
                  (setf lnc (- lnc 1))
                  (setf (getf (nth (- y 1) (nth (- x 1) grid)) :down) NIL)
                  (setf (getf (nth (- y 1) (nth (- x 1) grid)) :degree) (- (getf (nth (- y 1) (nth (- x 1) grid)) :degree) 1))
                  (setf (getf (nth (- y 1) (nth x grid)) :top) NIL)
                  (setf (getf (nth (- y 1) (nth x grid)) :degree) (- (getf (nth (- y 1) (nth x grid)) :degree) 1))
                  )
                )
               )
         )
        (
         (equal direc "R")
         (cond (
                (null (getf (nth y (nth (- x 1) grid)) :down))
                (progn
                  (setf lnc (+ lnc 1))
                  (setf (getf (nth y (nth (- x 1) grid)) :down) (list x y) )
                  (setf (getf (nth y (nth (- x 1) grid)) :degree) (+ (getf (nth y (nth (- x 1) grid)) :degree) 1))
                  (setf (getf (nth y (nth x grid)) :top) (list (- x 1) y))
                  (setf (getf (nth y (nth x grid)) :degree) (+ (getf (nth y (nth x grid)) :degree) 1))
                  )
                )
               (
                T
                (progn
                  (setf lnc (- lnc 1))
                  (setf (getf (nth y (nth (- x 1) grid)) :down) NIL)
                  (setf (getf (nth y (nth (- x 1) grid)) :degree) (- (getf (nth (- y 1) (nth (- x 1) grid)) :degree) 1))
                  (setf (getf (nth y (nth x grid)) :top) NIL)
                  (setf (getf (nth y (nth x grid)) :degree) (- (getf (nth (- y 1) (nth x grid)) :degree) 1))
                  )
                )
               )
         )
        )
  )

(defun check-cells ()
  (loop
    for i from 0 to (- num-rows 2)
    do (loop for j from 0 to (- num-cols 2)
             do (progn 
                  (setf max (nth j (nth i board)))
                  (cond 
                    (
                     (not (equal max -1))
                     (progn
                       (cond (
                              (not (null (getf (nth j (nth i grid)) :right)) )
                              (setf max (- max 1))
                              ))
                       (cond (
                              (not (null (getf (nth j (nth i grid)) :down)) )
                              (setf max (- max 1))
                              ))
                       (cond (
                              (not (null (getf (nth (+ j 1) (nth (+ i 1) grid)) :left)) )
                              (setf max (- max 1))
                              ))
                       (cond (
                              (not (null (getf (nth (+ j 1) (nth (+ i 1) grid)) :top)) )
                              (setf max (- max 1))
                              ))
                       (cond (
                              (not (zerop max))
                              (return-from check-cells NIL)
                              )
                             )
                       )
                     )
                    )
                  )
             
             )
    )
  (return-from check-cells T)
  )

(defun nxt-directions (curr-direc)
  (cond (
         (equal curr-direc "RIGHT")
         (list "RIGHT" "TOP" "DOWN")
         )
        (
         (equal curr-direc "DOWN")
         (list "LEFT" "DOWN" "RIGHT")
         )
        (
         (equal curr-direc "TOP")
         (list "LEFT" "RIGHT" "TOP")
         )
        (
         (equal curr-direc "LEFT")
         (list "LEFT" "TOP" "DOWN")
         )
        )
  )

(defun c-nxt-directions (curr-direc)
  (cond (
         (equal curr-direc "R")
         (list "R" "T" "D")
         )
        (
         (equal curr-direc "D")
         (list "L" "D" "R")
         )
        (
         (equal curr-direc "T")
         (list "L" "R" "T")
         )
        (
         (equal curr-direc "L")
         (list "L" "T" "D")
         )
        )
  )

(defun check-single-cell (i j) 
  ; (format t "~% checking cell ~A ~A ~%" i j)
  (setf max (nth j (nth i board)))
  (cond 
    (
     (equal max -1)
     (return-from check-single-cell T)
     )
    )
  
  (cond (
         (not (null (getf (nth j (nth i grid)) :right)) )
         (setf max (- max 1))
         ))
  ; (format t "~% Max : ~A ~%" max)
  (cond (
         (not (null (getf (nth j (nth i grid)) :down)) )
         (setf max (- max 1))
         ))
  (cond (
         (not (null (getf (nth (+ j 1) (nth (+ i 1) grid)) :left)) )
         (setf max (- max 1))
         ))
  (cond (
         (not (null (getf (nth (+ j 1) (nth (+ i 1) grid)) :top)) )
         (setf max (- max 1))
         ))
  ; (format t "~% Max now ~A ~%" max)
  (cond (
         (not (zerop max))
         (return-from check-single-cell T)
         )
        )
  (return-from check-single-cell NIL)
  )

(defun check-next (pointer nxt)
  ; (print "--1--")
  ; (print pointer)
  ; (print nxt)
  (setf xy (getf pointer (intern nxt :keyword)))
  (setf xx (nth 0 xy))
  ; (print "--2--")
  (setf yy (nth 1 xy))
  ; (print "--3--")
  (setf nxt-pointer (nth yy (nth xx grid)))
  ; (print "--4--")
  (setf cc 1)
  (loop 
    (if (eq nxt-pointer pointer) (return-from check-next cc))
    (progn
      (setf deg (getf nxt-pointer :degree))
      (setf nxt-dirs (nxt-directions nxt))
      (cond
        (
         (OR (OR (equal deg 1) (equal deg 3)) (equal deg 4))
         (return-from check-next -100000)
         )
        )   
      (loop for xnxt in nxt-dirs
            do (progn (cond 
                        (
                         (not (null (getf nxt-pointer (intern xnxt :keyword))))
                         (progn 
                           (setf cc (+ cc 1))
                           (setf nxt xnxt)
                           (setf xy (getf nxt-pointer (intern nxt :keyword)))
                           ; (print "--6--")
                           (setf xx (nth 0 xy))
                           ; (print "--7--")
                           (setf yy (nth 1 xy))
                           ; (print "--8--")
                           )
                         )
                        )
                      )
            
            )
      ; (print "--10--")
      (setf nxt-pointer (nth yy (nth xx grid)))
      ; (print "--11--")
      
      )
    
    )
  (return-from check-next cc)
  )



(defun check-complete-simple ()
  (setf directions '("RIGHT" "LEFT" "TOP" "DOWN"))
  (setf pointer NIL)
  (setf nxt NIL)
  (loop for trow in grid
        do (loop for tob in trow
                 do (progn 
                      (setf deg (getf tob :degree))
                      (cond (
                             (not (or (zerop deg) (equal deg 2)))
                             (return-from check-complete-simple NIL)
                             )
                            )
                      )
                 )
        )
  (return-from check-complete-simple T)
  )



(defun check-complete ()
  (setf check-lnc 0)
  (setf directions '("RIGHT" "LEFT" "TOP" "DOWN"))
  (setf pointer NIL)
  (setf nxt NIL)
  (loop for trow in grid
        do (loop for tob in trow
                 do (progn 
                      (setf deg (getf tob :degree))
                      (cond (
                             (not (or (zerop deg) (equal deg 2)))
                             (return-from check-complete NIL)
                             )
                            (
                             (not (zerop deg))
                             (progn 
                               (setf pointer tob)
                               (loop for dir in directions
                                     do (progn
                                          (setf tx (getf tob (intern dir :keyword)))
                                          (cond (
                                                 (not (null tx))
                                                 (progn
                                                   (setf nxt dir)
                                                   (setf res (check-next pointer nxt))
                                                   (cond 
                                                     (
                                                      (equal res lnc)
                                                      (return-from check-complete T)
                                                      )
                                                     (
                                                      T
                                                      (return-from check-complete NIL)
                                                      )
                                                     )
                                                   )
                                                 )
                                                )
                                          )
                                     )
                               )
                             )
                            )
                      )
                 )
        )
  )

(defun greeting ()
  (format t "
============================= SLITHER ================================

The aim is to make a single continuous loop around the board without 
crossing and if number is present in the cell, have exactly that
number of lines around that cell.

Specify the moves one by one as triplets. 

To draw a line on top of cell with position 1,2
=> 1 2 T
Here 1 2 specify the position of the cell and T specify the location 
to draw the line.
T => Top of the cell
D => Down / Below of the cell
L => Left of the cell
R => Right of the cell

Similarly, to remove a line present on top of cell with position 1,2
=> 1 2 T

To Quit, type Q

Choose an input board from the files in the folder or save a new file
with an input board then choose.

The program will notify whether you have solved or not the board at 
each step.

======================================================================
")
  )

(defun getf-string-equal (plist indicator)
  (loop
    for (i v) on plist by #'cddr
    when (string-equal i indicator)
    return v))

(defun select-start-node ()
  (loop for i from 0 to (- num-rows 2)
        do (loop for j from 0 to (- num-cols 2)
                 do (progn
                      (cond (
                             (equal (nth j (nth i board)) 3)
                             (return-from select-start-node (list i j))
                             )
                            )
                      )
                 )
        )
  (loop for i from 0 to (- num-rows 2)
        do (loop for j from 0 to (- num-cols 2)
                 do (progn
                      (cond (
                             (equal (nth j (nth i board)) 2)
                             (return-from select-start-node (list i j))
                             )
                            )
                      )
                 )
        )
  (loop for i from 0 to (- num-rows 2)
        do (loop for j from 0 to (- num-cols 2)
                 do (progn
                      (cond (
                             (equal (nth j (nth i board)) 1)
                             (return-from select-start-node (list i j))
                             )
                            )
                      )
                 )
        )
  
  )

; (defun nr-auto-solve (x y nxt-dir)
;   (setq solved NIL)
;   (setf queue (list ))
;   (loop while (null solved) do
        
;         )
;      )

(defun auto-solve (x y nxt-dir)
  (let ((p 0)(q 0)(nxt-directions '())(x1 0)(x2 0)(y1 0)(y2 0))
  ; (display-grid)
  ; check for out of bound conditions like top of 0,0; left of leftmost element, etc and return NIL 
  ; for such cases
  ; (format t "solving node ~A ~A ~A      " x y nxt-dir)
  (cond
    ((or 
       (and (equal "L" nxt-dir) (equal y 0))
       (and (equal "R" nxt-dir) (equal y (- num-cols 1)))
       (and (equal "T" nxt-dir) (equal x 0))
       (and (equal "D" nxt-dir) (equal x (- num-rows 1)))
       )
     (return-from auto-solve NIL)
      )
    )
  
  ; check if the adjacent cells' have been completly satified or not
  ; if any one satisfied, we cant draw a line, go back, return NIL
  ; else continue
  (cond
    ((equal nxt-dir "D") (progn (setq x1 x) (setq x2 x) (setq y1 (- y 1)) (setq y2 y)))
    ((equal nxt-dir "T") (progn (setq x1 (- x 1)) (setq x2 (- 1 x)) (setq y1 (- y 1)) (setq y2 y)))
    ((equal nxt-dir "R") (progn (setq x1 (- x 1)) (setq x2 x) (setq y1 y) (setq y2 y)))
    ((equal nxt-dir "L") (progn (setq x1 (- x 1)) (setq x2 x) (setq y1 (- y 1)) (setq y2 (- y 1))))
    )
  
  ; (format t "x1 y1 ~A ~A x2 y2 ~A ~A " x1 y1 x2 y2)
  
  (cond 
    (
     (and (>= x1 0) (>= y1 0) (< x1 (- num-rows 1)) (< y1 (- num-cols 1)))
     (cond
       ((null (check-single-cell x1 y1))
        (return-from auto-solve NIL)
        )
       )
     )
    )
  (cond
    (
     (and (>= x2 0) (>= y2 0) (< x2 (- num-rows 1)) (< y2 (- num-cols 1)))
     (cond
       ((null (check-single-cell x2 y2))
        (return-from auto-solve NIL)
        )
       )
     )
    )
  
  ; (format t "What to do")
  
  ; increment the degree of the current node and the node you will reach after navigating
  ; in the given direction;
  (cond
    ((equal nxt-dir "D") (progn (setf p (+ x 1)) (setf q y)))
    ((equal nxt-dir "T") (progn (setf p (- x 1)) (setf q y)))
    ((equal nxt-dir "R") (progn (setf q (+ y 1)) (setf p x)))
    ((equal nxt-dir "L") (progn (setf q (- y 1)) (setf p x)))
    )
  
  (setf cur-degree (getf (nth y (nth x grid)) :degree))
  (setf nxt-degree (getf (nth q (nth p grid)) :degree))
  ; (format t "~% Degree of the current node ~A ~A is ~A ~%" x y cur-degree)
  ; (format t "~% Degree of the current node ~A ~A is ~A ~%" p q nxt-degree)
  (setf check-for-complete NIL)
  (cond 
    ((equal nxt-degree 1) (setf check-for-complete T))
    ((equal nxt-degree 2) 
     (progn 
       ; (remove-edge x y nxt-dir p q)
       (return-from auto-solve NIL)))
    )
  
  (construct-edge x y nxt-dir p q)
  
  ; (setf c-degree (getf (nth y (nth x igrid)) :degree))
  ; (setf n-degree (getf (nth q (nth p igrid)) :degree))
  ; (format t "~% After adding node ~%")
  ; (format t "~% Degree of the current node ~A ~A is ~A ~%" x y c-degree)
  ; (format t "~% Degree of the current node ~A ~A is ~A ~%" p q n-degree)
  ; If the degree of the next node becomes 2, ask for a complteness check
  ;        if complete, return T, else remove the line and decrement the degrees, return NIL
  ; else call recursively for all directions, except for one which has degree 1, on the next node
  ; (print "Here")
  (cond 
    (check-for-complete
      (cond ((AND (check-cells) (check-complete-simple))
             (progn
               (setf is-solved T)
               (return-from auto-solve T)
               )
             )
            (T (progn
                 (remove-edge x y nxt-dir p q)
                 (return-from auto-solve NIL)
                ))
            )
      )
    )
  ; (print "THere")
  (setq nxt-directions (c-nxt-directions nxt-dir))
  ; (setq count 1)
  ; (loop for direction in nxt-directions
  ;       do (progn 
  ;            (format t "~%~A Calling with ~A ~A ~A ~%" count p q direction)
  ;            (setq xp p)
  ;            (setq xq q)
  ;            (cond (
  ;                 (auto-solve p q direction)
  ;                 (return-from auto-solve T)
  ;                 )
  ;                )
  ;            ; (format t  "~%~A ~A : ~A ~A~%" p q xp xq)
  ;            (setq p xp)
  ;            (setq q xq)
  ;            (setq count (+ count 1))
  ;            (cond 
  ;              (
  ;               (equal count 4)
  ;               (progn (remove-edge x y nxt-dir p q)
  ;               (return-from auto-solve NIL))
  ;               )
  ;              )
  ;            )
  ;       )
  (cond (
         (auto-solve p q (nth 0 nxt-directions))
         (return-from auto-solve T)
      )
    )
  (cond (
         (auto-solve p q (nth 1 nxt-directions))
         (return-from auto-solve T)
      )
    )
  (cond (
         (auto-solve p q (nth 2 nxt-directions))
         (return-from auto-solve T)
      )
     )
  (progn (remove-edge x y nxt-dir p q))
  (return-from auto-solve NIL)
  ))

(defun slither()
  (greeting)
  (setf lnc 0)
  (format t "Enter input filepath : ")
  (setq filepath (read-line))
  (setq board '())
  (let ((in (open filepath :if-does-not-exist nil)))
    (when in (setq num-cols (ceiling (/ (length (read-line in)) 2)))
      (setq lineread 0)
      (loop for line = (read-line in nil)
            while line do (progn (if (evenp lineread) (setf board (append board (list (add-row line)))))
                                 (setq lineread (+ lineread 1)))
            )
      (close in)))
  (setf num-rows (ceiling (/ (+ lineread 1) 2)))
  (init-grid)
  (setf ins '(1 2 "U"))
  (setq start-nodes (select-start-node))
  ; (format t "start node ~A ~A " (nth  0 start-nodes) (nth 1 start-nodes))
  ; (print (auto-solve (nth  0 start-nodes) (nth 1 start-nodes) "D"))
  (display-grid)
  (format t "~%~A" "Specify Manual (M) or Automatic (A) : ")
  (setf mode (read-line t))
  (if (equal mode "A")
      (progn
        (format t "~%Solving! Please wait....~%")
        (setf start-time (get-internal-run-time))
        (setf is-solved NIL)
               
                (auto-solve (nth  0 start-nodes) (nth 1 start-nodes) "D")
                (if (null is-solved) 
                  (auto-solve (nth  0 start-nodes) (nth 1 start-nodes) "T"))
                (if (null is-solved) 
                  (auto-solve (nth  0 start-nodes) (nth 1 start-nodes) "R"))
                (if (null is-solved) 
                  (auto-solve (nth  0 start-nodes) (nth 1 start-nodes) "L"))
                
              
        (setf end-time (get-internal-run-time))
        (setf diff (- end-time start-time))
        (setf cpu-sec (/ (float diff) (float INTERNAL-TIME-UNITS-PER-SECOND)))
        (if is-solved
            (progn
              (display-grid)
              (format t "~%=== Solved successfully in ~A internal run time i.e. ~A CPU Seconds ===~%" diff cpu-sec)
              (return-from slither)
              )
            )
        (format t "~%Sorry, the board could not be solved.~%")
        (return-from slither)
        )
      )
  (loop
    (format t "~A" "Enter command or Q to quit: ")
    (setf ins (read-line t))
    (if (equal ins "Q") (return))
    (setf ins (space-split ins))
    (draw-line (parse-integer (nth 0 ins)) (parse-integer (nth 1 ins)) (nth 2 ins))
    (display-grid)
    (cond (
           (AND (check-cells) (check-complete))
           (progn (format t "Congrats! You have solved it.")
                  (return))
           )
          (
           T
           (format t "~%Not solved yet")
           )
          )
    (format t "~%")
    )
  )