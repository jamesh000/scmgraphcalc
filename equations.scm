;;; Lexer stuff
(define (next-string string token-length)
  (substring string token-length))

(define (get-current-char string)
  (string-ref string 0))

(define-syntax add-token-and-continue
  (sc-macro-transformer
   (lambda (form environment)
     (let ((equation-string (close-syntax (cadr form) environment)))
	   (symbol (close-syntax (caddr form) environment)))
	   (if (eqv? (cdddr form) ())
	       `(cons ,symbol (lex (next-string ,string 1)))
	       `(cons (cons ,symbol (cadddr form))
		       (lex (next-string ,string 1)))))))

(define-syntax add-number-and-continue
  (sc-macro-transformer
   (lambda (form environment)
     (let ((equation-string (close-syntax (cadr form) environment))
	   (number (close-syntax (caddr form) environment)))
       `(cons (cons 'NUM (string->number ,number))
	      (lex (next-string ,string	(string-length ,number))))))))

(define (lex equation-string)
  (let ((char (get-current-char equation-string)))
    (if (char->digit char)
	(let ((num (get-num equation-string)))
	  (add-number-and-continue equation-string num))
	(case char
	  ((#\=) (add-token-and-continue equation-string 'EQ))
	  ((#\x) (add-token-and-continue equation-string 'IND))
	  ((#\y) (add-token-and-continue equation-string 'DEP))
	  ((#\() (add-token-and-continue equation-string 'OPEN))
	  ((#\)) (add-token-and-continue equation-string 'CLOSE))
	  ((#\+) (add-token-and-continue equation-string 'OP1 +))
	  ((#\-) (add-token-and-continue equation-string 'OP1 -))
	  ((#\*) (add-token-and-continue equation-string 'OP2 *))
	  ((#\/) (add-token-and-continue equation-string 'OP2 /))
	  ((#\^) (add-token-and-continue equation-string 'EXPT))
	  ((#\v) (add-token-and-continue equation-string 'ROOT))
	  ((#\l) (add-token-and-continue equation-string 'LOG))
	  ((#\n) (add-token-and-continue equation-string 'LN))
	  (else (format t "Invalid character ~a in equation" char))))))

(define (get-num string)
  (define (iter string end)
    (if (char->digit (string-ref string end))
	(iter string (+ end 1))
	(substring string 0 end)))
  (iter string 1))


;;; Parser stuff

