;;; This file contains the drawing operations

;;; Functions for manipulating the window.
(define (set-window-bounds x-min y-min x-max y-max)
  (graphics-set-coordinate-limits *window* x-min y-min x-max y-max))

(define (clear-window)
  (graphics-clear *window*))

;;; Draw plots.
(define (draw-plot plot)
  (clear-window)
  (set-window-bounds (x-min plot) (y-min plot)
		     (x-max plot) (y-max plot))
  (draw-axes plot)
  (draw-functions plot))

;;; function for drawing axes
(define (draw-axes plot)
  (when (axes? plot)
    (graphics-draw-line *window* (x-min plot) 0 (x-max plot) 0)
    (graphics-draw-line *window* 0 (y-min plot) 0 (y-max plot))
    (draw-decor plot)))

(define-syntax draw-decor-for-axis
  (sc-macro-transformer
   (lambda (form senv)
     (let* ((plot (close-syntax (cadr form) senv))
	    (axis (symbol (caddr form)))
	    (first-start (if (eqv? axis 'x) x-min y-min))
	    (first-end (if (eqv? axis 'x) x-max y-max))
	    (second-start (if (eqv? first-start x-min) y-min x-min))
	    (second-end (if (eqv? first-end x-max) y-max x-max)))
     `(let* ((start (,first-start ,plot))
	     (end (,first-end ,plot))
	     (axis-length (- end start))
	     (half-mark-length (/ (- (,second-end ,plot) (,second-start ,plot)) 180)))
	(if (marked? ,plot)
	    (do-at-intervals (lambda (position)
			       (draw-axis-mark ',axis position half-mark-length))
			     (find-step axis-length)
			     start end))
	(if (numbered? ,plot)
	    (do-at-intervals (lambda (position)
			       (draw-axis-number ',axis position (* half-mark-length 2)))
			     (* 10 (find-step axis-length))
			     start end)))))))

(define (draw-decor plot)
  (draw-decor-for-axis plot x)
  (draw-decor-for-axis plot y))

(define (do-at-intervals f inc start end)
  (define (iter position)
    (when (< position end)
      (if (> position start)
	  (f position))
      (iter (+ position inc))))
  (iter start))

(define (draw-axis-mark axis position half-length)
  (apply graphics-draw-line
	 *window*
	 (if (eqv? axis 'x)
	     (list position (- half-length)
		   position half-length)
	     (list (- half-length) position
		   half-length position))))

(define (draw-axis-number axis position offset)
  (if (not (= position 0))
      (apply graphics-draw-text
	     *window*
	     (append (if (eqv? axis 'x)
			 (list position offset)
			 (list offset position))
		     (list (number->string position))))))

(define (draw-y-axis-number position)
  (if (not (= position 0))
      (graphics-draw-text *window* 0 position (number->string position))))

(define (find-step x)
  (if (> x 1)
      (* 10 (find-step (/ x 10)))
      (/ 100)))
  

;;; Functions for drawing functions
(define (draw-functions plot)
  (define (iter function-num inc length)
    (when (< function-num length)
      (let ((function (get-function plot function-num)))
	(if function
	    (draw-function function inc
			   (x-min plot)
			   (y-min plot)
			   (x-max plot)
			   (y-max plot))))
      (iter (+ 1 function-num) inc length)))
  (iter 0
	(/ (- (x-max plot) (x-min plot)) 20000)
	(vector-length (get-function-list plot))))

(define (draw-function f inc x-min y-min x-max y-max)
  (define (df-iter x)
    (graph x (f x) y-min y-max)
    (if (<= x x-max)
	(df-iter (+ x inc))))
  (df-iter x-min))

(define (graph x y y-min y-max)
  (if (pair? y)
      (let ((y1 (car y))
	    (y2 (cdr y)))
	(if (graphable? y1 y-min y-max)
	    (graph-point x y1))
	(if (graphable? y2 y-min y-max)
	    (graph-point x y2)))
      (if (graphable? y y-min y-max)
	  (graph-point x y))))

(define (graph-point x y)
  (graphics-draw-point *window* x y))

(define (graphable? y y-min y-max)
  (and (real? y)
       (<= y-min y y-max)))
