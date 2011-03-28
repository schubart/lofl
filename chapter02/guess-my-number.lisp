; You think of a number, the computer "guesses" it:
; In REPL:
; (load "guess-my-number.lisp")
; (guess-my-number)
; (smaller) or (bigger) until your number is printed.

(defparameter *small* 1)

(defparameter *big* 100)

(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))
