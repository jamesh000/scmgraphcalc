;;; These are the plot management utilities
;;; Design overview:
;;; There is a core set of functions for dealing with the plots themselves
;;; that will be used by outside functions to edit the plots.
;;; There is another set of functions that assist in generating the plot
;;; representations that will be used by outside functions to add and edit plots.
;;; Finally, there is a set of functions that extract data from a plot.
;;; Outside functions that need information on plots will use the function 'get-plot'
;;; and then the various extracting functions to extract the data they need.

;;; Functions for dealing with the plots themselves

(define (add-plot plot-name)
  (hash-table-set! *plots*
		   plot-name
		   (list (make-vector 0)
			 (make-bounds -10 -10 10 10)
			 (make-axes-settings #t #t #t))))

(define (get-plot plot-name)
  (hash-table-ref *plots* plot-name))

(define (remove-plot plot-name)
  (hash-table-delete! *plots* plot-name))

(define (plot-exists? plot-name)
  (hash-table/lookup *plots*
		     plot-name
		     (lambda (x) #t)
		     (lambda () #f)))

(define (list-plots)
  (hash-table-keys *plots*))

(define (set-bounds plot-name x-min y-min x-max y-max)
  (list-set! (get-plot plot-name) 1 (make-bounds x-min y-min x-max y-max)))

(define (set-plot-axes-settings plot-name axes? marked? numbered?)
  (list-set! (get-plot plot-name) 2 (make-axes-settings axes? marked? numbered?)))

(define (add-function plot-name function)
  (let ((plot (get-plot plot-name)))
    (list-set! plot 0 (grow-function-list (get-function-list plot)))
    (vector-set! (get-function-list plot)
		 (- (vector-length (get-function-list plot)) 1)
		 function)))

(define (grow-function-list function-list)
  (vector-grow function-list (+ 1 (vector-length function-list))))

(define (edit-function plot-name function-num new-function)
  (vector-set! (get-function-list (get-plot plot-name))
	       function-num
	       new-function))

(define (remove-function plot-name function-num)
  (edit-function plot-name function-name #f))

;;; Functions for generating the plots

(define (make-axes-settings axes? marks? numbered?)
  (list axes? marks? numbered?))

(define (make-bounds x-min y-min x-max y-max)
  (list x-min y-min x-max y-max))


;;; Functions for extracting data from the plots

(define (get-function-list plot)
  (car plot))

(define (get-function plot function-num)
  (vector-ref (get-function-list plot) function-num))

(define (axes? plot)
  (car (caddr plot)))
(define (marked? plot)
  (cadr (caddr plot)))
(define (numbered? plot)
  (caddr (caddr plot)))

(define (x-min plot)
  (car (cadr plot)))
(define (y-min plot)
  (cadr (cadr plot)))
(define (x-max plot)
  (caddr (cadr plot)))
(define (y-max plot)
  (cadddr (cadr plot)))
