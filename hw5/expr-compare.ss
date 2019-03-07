#lang racket

; Tests if x starts with a keyword 'if' 'lambda' or 'λ', while y doesn't
(define (missing-keyword x y)
	(and 
		(or (equal? (car x) 'if) (equal? (car x) 'lambda) (equal? (car x) 'λ))
		(not (or (equal? (car y) 'if) (equal? (car y) 'lambda) (equal? (car y) 'λ)))
	)
)

; Tests if x or y has special form λ while the other has lambda
(define (check-lambdas x y)
	(or 
		(and (equal? (car x) 'λ) (equal? (car y) 'lambda))
		(and (equal? (car x) 'lambda) (equal? (car y) 'λ))
	)
)

; Tests that both x and y start with lambda
(define (matching-lambdas x y)
	(and 
		(or (equal? (car x) 'λ) (equal? (car x) 'lambda))
		(or (equal? (car y) 'λ) (equal? (car y) 'lambda))
	)
)

; Tests if x is either lambda or λ
(define (is-lambda x)
	(or (equal? x 'λ) (equal? x 'lambda))
)

; Parses x and y to bind variables at same location together
; Returns bindings in form of list of two lists
; First list contains the binded variables in list x
; Second list contains the binded variables in list y
(define (get-bindings x y)
	(cond 
		[(equal? x y) (list '() '())]
		[(and (symbol? x) (symbol? y)) (list (list x) (list y))]
		[(and (list? x) (list? y) (or (empty? x) (empty? y))) (list '() '())]
		[(and (list? x) (list? y)) (let (
			[head-bindings (get-bindings (car x) (car y))]
			[tail-bindings (get-bindings (cdr x) (cdr y))])
			(list (append (first head-bindings) (first tail-bindings)) (append (second head-bindings) (second tail-bindings)))
		)]
		[else (list '() '())]
	)
)

; Take symbol x, and returns the bounded version, i.e. x!y
; If resolve-first is true, check from first list
; If false, resolve from second list
(define (resolve-bindings x bindings resolve-first)
	(cond 
		[(and resolve-first (equal? x (car (first bindings))))
			(string->symbol (string-append (symbol->string (car (first bindings))) "!" (symbol->string (car (second bindings)))))]
		[(and (not resolve-first) (equal? x (car (second bindings))))
			(string->symbol (string-append (symbol->string (car (first bindings))) "!" (symbol->string (car (second bindings)))))]
		[else (resolve-bindings x (list (cdr (first bindings)) (cdr (second bindings))) resolve-first)]
	)
)

; Replaces variables in x and y with their bounded version
; Returns two lists, with first list corresponding to x, second with y
; Only replaces at the current lambda level
(define (bind x y bindings)
	(cond 
		[(and (symbol? x) (symbol? y) (member x (first bindings)) (member y (second bindings))) 
			(list (resolve-bindings x bindings #t) (resolve-bindings y bindings #f))]
		[(and (symbol? x) (symbol? y) (member x (first bindings))) 
			(list (resolve-bindings x bindings #t) y)]
		[(and (symbol? x) (symbol? y) (member y (second bindings))) 
			(list x (resolve-bindings y bindings #f))]
		[(and (list? x) (not (empty? x)) (not (is-lambda (car x))) (list? y) (not (empty? y)) (not (is-lambda (car y)))) (let (
			[binded-head (bind (car x) (car y) bindings)]
			[binded-tail (bind (cdr x) (cdr y) bindings)])
			(list (cons (first binded-head) (first binded-tail)) (cons (second binded-head) (second binded-tail)))
		)]
		[else (list x y)]
	)
)

; Binds x and y variables and then compares their difference
(define (parse-lambda-bindings x y)
	(let* (
		[bindings (get-bindings (car x) (car y))]
		[bound (bind x y bindings)]
		)
		(compare-diff (first bound) (second bound))
	)
)

; Checks basic lambda arguments are satisfied
(define (lambda-arg-check x y)
	(or (and (empty? x) (empty? y))
		(and
			(equal? (symbol? x) (symbol? y))
			(equal? (list? x) (list? y))
			(equal? (pair? x) (pair? y))
			(cond 
				[(or (and (list? x) (list? y)) (and (pair? x) (pair? y))) 
				(and 
				 	(equal? (length x) (length y))
				 	(lambda-arg-check (car x) (car y))
				 	(lambda-arg-check (cdr x) (cdr y))
				)]
			)
		)
	)
)

; Helper function for expr-compare that compares two expressions, and finds the differences between them
(define (compare-diff x y)
	(cond
		[(equal? x y) y]
		[(and (boolean? x) (boolean? y)) (if x '% '(not %))]
		[(and (list? x) (list? y) (check-lambdas x y)) (compare-diff (cons 'λ (cdr x)) (cons 'λ (cdr y)))]
		[(and (list? x) (list? y) (matching-lambdas x y) (lambda-arg-check (cadr x) (cadr y))) (cons (compare-diff (car x) (car y)) (parse-lambda-bindings (cdr x) (cdr y)))]
		[(or 
			(and (list? x) (list? y) (matching-lambdas x y) (not (lambda-arg-check (cadr x) (cadr y))))
			(not (list? x))
			(not (list? y))
			(not (equal? (length x) (length y)))
			(missing-keyword x y)
			(missing-keyword y x)
			(equal? 'quote (car x))
			(equal? 'quote (car y)))
			`(if % ,x ,y)]
		[else (cons (compare-diff (car x) (car y)) (compare-diff (cdr x) (cdr y)))]
	)
)

(define (expr-compare x y)
	(compare-diff x y)
)

(define (test-expr-compare x y)
  	(and 
  		(equal? (eval x) (eval `(let ([% #t]) ,(expr-compare x y)))) 
  		(equal? (eval y) (eval `(let ([% #f]) ,(expr-compare x y))))
	)
)

(define test-expr-x '(if #f ((lambda (a b c d) (list (* b d) (+ c 12) (if (> a 0) ((λ (a b) (+ a b)) 1 3) 13))) 1 2 15 10) (list (member 10 (quote (1 4 10 15))) #f)))

(define test-expr-y '(if #t ((lambda (b d c a) (list (* b c) (+ b 2) (if (< a 4) ((lambda (a x) (+ x a)) 39 9) 1))) 13 26 5 1) (append (member 10  '(31 42 3 1)) #t)))