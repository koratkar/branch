;;; Height: 6 
;;; Width: 7


; - no moves greater than 7 in width 
; - once slot is filled, not legal for moves 
; - check for wins, use what-is
; - test cases

(defvar gb 
  '((○ ○ ○ ○ ○ ○ ○)
    (○ ○ ○ ○ ○ ○ ○)
    (○ ○ ○ ○ ○ ○ ○)
    (○ ○ ○ ○ ○ ○ ○)
    (○ ○ ○ ○ ○ ○ ○)
    (○ ○ ○ ○ ○ ○ ○)))

(defvar red '◉)
(defvar yellow '*)


(defun display-board (l)
  (let ((b (reverse l)))
    (progn 
      (loop for row in b
      do (progn 
            (print row)))
      (fresh-line)
      (write-line "+++++++++++++++")
      (write-line " 1 2 3 4 5 6 7 "))))

(defun gen-board (game-board new-t-column token)
  (let ((new-t-row 1))
    (loop 
      for row in game-board
      for i from 1 to (length game-board)
      collect 
        (if (= i new-t-row)
          (loop  
            for x in row
            for c from 1 to (length row)
            collect (cond ((and (= c new-t-column) (equal x '○)) token)
                          ((and (= c new-t-column) (not (equal x '○))) (progn (incf new-t-row) x))
                          (t x)))
            row))))

(defun what-is (game-board row collumn)
  (nth (- collumn 1) (nth (- row 1) game-board)))

(defun win? (board row collumn)
  (progn (list board row collumn)
    T))

(defun print-turn (n)
  (if (oddp n)
    (write-line "RED TURN >>")
    (write-line "YELLOW TURN >>")))

(defun game-loop (board turn)
  (labels ((try-again (board turn input)
              (cond ((equal input "quit")
                       (progn (write-line "THANKS FOR PLAYING")
                              (quit)))
                    ((equal input "clear")  
                              (game-loop gb 1))
                    (t (progn (write-line "INPUT NOT VALID") 
                              (game-loop board turn)))))
            (sub8-and-number (input-number)
              (if (numberp input-number)
                  (if (> input-number 7) 
                      NIL
                      T)
                  NIL)))

          (progn 
            (display-board board)
            (print-turn turn) 
            (let* ((user-input (read-line))
                  (par-input (parse-integer user-input :junk-allowed t)))
              (if (sub8-and-number par-input)
                (if (oddp turn)
                  (game-loop (gen-board board par-input red) (1+ turn))
                  (game-loop (gen-board board par-input yellow) (1+ turn)))
                (try-again board turn user-input))))))


(game-loop gb 1)
