(load "../chapter05/adventure.lisp")

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-repl ()
  ; Prompt.
  (princ "> ")
  ; Read command.
  (let ((command (game-read)))
    (cond ((eq (car command) 'quit) ; Quit?
           (princ "Good bye."))

          ((null (car command)) ; Ignore empty input.
           (game-repl))

          (t ; Default: Evaluate and start over.
           (game-print (game-eval command))
           (game-repl)))))

; Like read, but wraps everything in a list and quotes all but first elements.
(defun game-read ()
  ; read-line, wrap parentheses around it. Use read-from-string to turn this
  ; string into a list.
  (let ((command (read-from-string
                  (concatenate 'string "(" (read-line) ")"))))
    ; Helper for turning x into (quote x), a.k.a. 'x
    (flet ((quote-it (x)
             (list 'quote x)))
      ; Return first symbol unquoted, rest quoted, e.g. (walk 'west)
      (cons (car command)
            (mapcar #'quote-it (cdr command))))))

; Like eval, but restricts to certain commands.
(defun game-eval (sexp)
  ; Is the first element of the sexp an allowed command?
  (if (member (car sexp) *allowed-commands*)
      ; Then do it.
      (eval sexp)
      ; Else give error message.
      '(i do not know how to do that.)))

; Capitalize first letter of first word of each sentence. I did not bother
; implementing the "literals" function suggested in the book.
(defun tweak-text (lst caps)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\space)
             ; Space: Keep as is.
             (cons item (tweak-text rest caps)))

            ((member item '(#\! #\? #\.))
             ; End of sentence. Keep and switch to caps for next word.
             (cons item (tweak-text rest t)))

            (caps
             ; Caps mode. Capitalize next character and turn off caps mode.
             (cons (char-upcase item) (tweak-text rest nil)))

            (t
             ; Default: Downcase next character and turn off caps mode.
             (cons (char-downcase item) (tweak-text rest nil)))))))

; Back and forth between lists and strings...
(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t)
                 'string))
  (fresh-line))
;(game-repl)