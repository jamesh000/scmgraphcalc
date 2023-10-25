
;;; Add the commands
(define (add-command character function description)
  (hash-table-set! *commands* character (cons function description)))

(define (add-commands)
    (add-command #\? print-help "print the help information")
    (add-command #\a prompt-add-plot "Add a plot")
    (add-command #\b prompt-set-bounds "set the Bounds of the current plot")
    (add-command #\d prompt-remove-function "remove (Delete) a function")
    (add-command #\e eval-input "Evaluate an input expression")
    (add-command #\f prompt-add-function "add a Function")
    (add-command #\l print-functions "List functions")
    (add-command #\p print-plots "Print the plots")
    (add-command #\r prompt-remove-plot "Remove a plot")
    (add-command #\s prompt-set-plot "Switch plots")
    (add-command #\q prompt-quit-calc "Quit the calculator")
    (add-command #\w redraw-current-plot "redraW the current plot"))


;;; Procedures for manipulating the commands.
(define (get-command character)
  (hash-table-ref/default *commands* character #f))

(define (command-exists? character)
  (hash-table/lookup *commands*
		     character
		     (lambda (x) #t)
		     (lambda () #f)))

(define (run-command command-data)
  (apply (car command-data)))

(define (get-command-description command-data)
  (cdr command-data))

(define (read-command-char)
  (prompt-for-command-char "Enter a command: "))


;;; The command procedures themselves.

(define (print-help)
  (newline)
  (hash-table-walk *commands*
		   (lambda (character data)
		     (format #t "~@4a   ~a~%"
			     (char-upcase character)
			     (get-command-description data)))))

;;; The plot command procedures.

(define-syntax plot-operation
  (sc-macro-transformer
   (lambda (form environment)
     (let ((prompt (close-syntax (cadr form) environment))
	   (exists-form (make-sc-list environment '(requery plot-name) (caddr form)))
	   (else-form (make-sc-list environment '(requery plot-name) (cadddr form))))
       `(let requery ()
	  (let ((plot-name (prompt-for-string ,prompt)))
	    (cond ((string=? plot-name "quit") (format #t "~%Quiting.~%"))
		  ((plot-exists? plot-name) ,@exists-form)
		  (else ,@else-form))))))))

(define (make-sc-list environment free-names form)
  (map (lambda (exp)
	 (make-syntactic-closure environment free-names exp))
       form))

(define (prompt-add-plot)
  (plot-operation
   "Name for new plot: "
   ((format #t "Plot ~a already exists~%" plot-name)
    (requery))
   ((format #t "Adding plot ~a.~%" plot-name)
    (add-plot plot-name)
    (set-plot plot-name))))

(define (print-plots)
  (newline)
  (format #t "Plots:~%")
  (map (lambda (plot-name)
	 (format #t "~a~%" plot-name))
       (list-plots)))

(define (prompt-remove-plot)
  (plot-operation
   "Remove plot: "
   ((when (prompt-for-confirmation (format #f "Really delete plot ~a" plot-name))
      (format #t "~%Removing plot ~a.~%" plot-name)
      (remove-plot plot-name)))
   ((format #t "Plot ~a does not exists.~%")
    (requery))))

(define (prompt-set-plot)
  (plot-operation
   "Switch to plot: "
   ((cond ((string=? plot-name *current-plot*)
	   (format #t "Already on plot ~a.~%" plot-name)
	   (requery))
	  (else (format #t "~%Switching to plot ~a." plot-name)
		(set-plot plot-name))))
   ((format #t "Plot ~a does not exist.~%" plot-name)
    (requery))))

(define (set-plot plot-name)
  (set! *current-plot* plot-name)
  (clear-window)
  (draw-plot (get-plot plot-name)))


;;; Function command procedures

(define (print-functions)
  (newline)
  (vector-map (lambda (function) (pp function))
	      (get-function-list (get-plot *current-plot*))))

(define (prompt-add-function)
  (let* ((input (prompt-for-command-expression "Enter a Scheme expression that evaluates to a function:\n"))
	 (function (eval input *environment*)))
    (cond ((procedure? function)
	   (add-function *current-plot* function))
	   (else (fresh-line)
		 (format #t "The expression ~a did not evaluate to a function." input)))))

(define (prompt-remove-function)
  (let ((function-num (string->number (prompt-for-string "Enter a function number: "))))
    (remove-function *current-plot* function-num)))

;;; Plot settings procedures.

(define (prompt-set-bounds)
  (let ((y-min (prompt-for-number "Enter the Y minimum: "))
	(x-max (prompt-for-number "Enter the X maximum: "))
	(y-max (prompt-for-number "Enter the Y maximum: "))
	(x-min (prompt-for-number "Enter the X minimum: ")))
    (set-bounds *current-plot* x-min y-min x-max y-max)))

(define (prompt-for-number prompt)
  (string->number (prompt-for-string prompt)))

;;; Other procedures.
(define (prompt-quit-calc)
  (when (prompt-for-confirmation "Are you sure you would like to quit?")
    (graphics-close *window*)))

(define (redraw-current-plot)
  (format #t "~%Redrawing current plot.~%")
  (draw-plot (get-plot *current-plot*)))

(define (eval-input)
  (let ((input (prompt-for-command-expression "Enter a Scheme expression:\n")))
    (calc-eval input *environment*)))


(define-syntax calc-eval
  (sc-macro-transformer
   (lambda (form senv)
     (warn-all
      (with-input-loop-restart
       (lambda ()
	 (eval (close-syntax (cadr form) senv)
	       *environment*)))))))

(define (warn-all thunk)
  (bind-condition-handler
      '()
      (lambda (condition)
	(warn condition)
	(invoke-restart (find-restart 'input-loop)))
    thunk))

(define (with-input-loop-restart thunk)
  (lambda ()
    (with-restart
	'input-loop
	"Return to the input loop."
	(lambda ()
	  (input-loop))
	#f
      thunk)))
