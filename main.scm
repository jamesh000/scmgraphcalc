;;; This is the main file for the graphing calculator
;;; Load the options
(load-option 'format)
(load-option 'x11)

;;; load all the other things
(load "~/Code/scm/graphcalc/ui.scm")
(load "~/Code/scm/graphcalc/plots.scm")
(load "~/Code/scm/graphcalc/draw.scm")

;;; Do initialization.

;;; Create the window everything will be drawn in
(define *window* (make-graphics-device 'x))

;;; Create the hash table that will store the plots.
(define *plots* (make-string-hash-table))

;;; Create the hash table that will store the commands and
;;; add them.
(define *commands* (make-strong-eq-hash-table))
(add-commands)

;;; The current plot goes here.
(define *current-plot*)

;;; The environment things are executed in.
(define *environment* (make-top-level-environment))

;;; Start the input loop.
(define (input-loop)
  (let ((command-char (read-command-char)))
    (cond ((command-exists? command-char)
	   (run-command (get-command command-char))
	   (if (not (char-ci=? command-char #\q))
	       (input-loop)))
	   (else
	    (format #t "~%~a is not a command" command-char)))))

(input-loop)
