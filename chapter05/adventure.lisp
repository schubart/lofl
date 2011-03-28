;===========================================================================
; Data
;===========================================================================

; Location descriptions: Association list with
; key = location (symbol)
; value = description (list of symbols)
(defparameter *nodes* '((living-room (you are in the living-room. a wizard
                                      is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden. there is a
                                 well in front of you.))
                        (attic (you are in the attic. there is a giant
                                welding torch in the corner.))))

; Connections between locations: Association list with
; key = location (symbol)
; value = list of edges
; edge = triple: destination node, direction, connection
(defparameter *edges* '((living-room
                         (garden west door)
                         (attic upstairs ladder))
                        (garden
                         (living-room east door))
                        (attic
                         (living-room downstairs ladder))))

(defparameter *objects* '(whiskey
                          bucket
                          frog
                          chain))

; Object locations: Association list with
; key = object
; value = location
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))


(defparameter *location* 'living-room)

;===========================================================================
; Purely functional: Examine and describe world.
;===========================================================================

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-paths (location edges)
  (labels ((describe-path (edge)
             `(there is a ,(caddr edge) going ,(cadr edge) from here.)))
    ; Find edges from location, describe each, append descriptions.
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges))))))

(defun objects-at (location objects object-locations)
  ; Helper: Test if object is at location.
  (labels ((at-location-p (object)
             (eq (cadr (assoc object object-locations))
                 location)))
    ; Return all objects except the ones not at location.
    (remove-if-not #'at-location-p objects)))

(defun describe-objects (location objects object-locations)
  ; Helper: Describe an object.
  (labels ((describe-object (object)
             `(you see a ,object on the floor.)))
    ; Find objects at location, describe each, append descriptions.
    (apply #'append (mapcar #'describe-object
                            (objects-at location objects object-locations)))))

;===========================================================================
; Note purely functional. Manipulate global state.
;===========================================================================

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  ; Among edges from current location, find edge for direction (second element,
  ; cadr of edge triple.)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*)) ; Edges from location.
                    :key #'cadr)))
    ; Is there such an edge?
    (if next
        ; Then go to its destination and look around.
        (progn (setf *location* (car next))
               (look))
        ; Else give error message.
        '(you cannot go that way.))))

(defun pickup (object)
  ; Is the object at the current location?
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         ; Then store that object is now at pseudo-location 'body and tell user.
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        ; Else give error message.
        (t `(you cannot get that))))

(defun inventory ()
  ; Show all objects at location 'body.
  (cons `items- (objects-at 'body *objects* *object-locations*)))
