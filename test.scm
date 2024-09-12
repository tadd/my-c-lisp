(load "./libtest.scm")

(describe "parsing comments" (lambda ()
  (expect eqv? 1 1 ; foo
               )
  (expect eqv? 2 2 ;;bar
               )
  (expect equal? '(1 2) '(1 ;;; ?? ;;;
                          2))))

(describe "peculiar identifiers" (lambda ()
  (expect eq? '+ '+)
  (expect eq? '- '-)
  (expect eq? '.. '..)
  (expect eq? '... '...)))

;; 4.1. Primitive expression types
;; 4.1.2. Literal expressions
(describe "quote basic" (lambda ()
  (expect equal? '() (list))
  (expect equal? '(1) (list 1))
  (expect equal? '(1 2) (list 1 2))))

(describe "quote" (lambda ()
  (expect eqv? '10 10)
  (expect eq? '() ())
  (expect equal? '(1 2 3) (list 1 2 3))
  (expect eq? 'foooo (quote foooo))
  (let ((l '(+ 1 2)))
     (expect equal? l (quote (+ 1 2)))
     (expect equal? l (list (quote +) 1 2)))
  (expect equal? '(quote a) (list 'quote 'a))
  (expect equal? ''a (list 'quote 'a))
  (expect equal? '"abc" "abc")
  (expect equal? '#t #t)))

;; 4.1.4. Procedures
(describe "lambda" (lambda ()
  (expect procedure? (lambda () 1))
  (expect eqv? ((lambda () 42)) 42)
  (expect eqv? ((lambda (x) (* 2 x)) 21) 42)
  (expect eqv? ((lambda (x y) (* x y)) 3 14) 42)
  (expect eqv? (begin
                 (define mul (lambda (x y) (* x y)))
                 (mul 3 14)) 42)
  (expect eqv? (begin
                 (define a 42)
                 ((lambda () a))) 42)
  (expect eqv? (begin
                 (define a 42)
                 ((lambda ()
                    ((lambda () a))))) 42)
  (expect eqv? (begin
                 (define a 42)
                 ((lambda (a) a) 10)
                   a) 42)))

(describe "lambda2" (lambda ()
  (expect eqv? (begin
                 (define a 42)
                 (define f (lambda () a))
                 (define g (lambda () f))
                 ((g))) 42)
  (expect eqv? (begin
                 (define a 42)
                 (define f (lambda () a))
                 (((lambda () f)))) 42)
  (expect eqv? (begin
                 (define a 42)
                 (define f (lambda ()
                             (lambda () a)))
                 (define g (f))
                 (g)) 42)
  (expect eqv? (begin
                 (define a 42)
                 (((lambda ()
                     (lambda () a))))) 42)
  (expect eqv? (begin
                 (define a 42)
                 (define f (lambda () a))
                 ((((lambda ()
                      (lambda () f)))))) 42)
  (expect eqv? (((lambda ()
                   42
                   (lambda () 42)))) 42)
  (expect eqv? ((((lambda ()
                    42
                    (lambda ()
                      (lambda () 42)))))) 42)))

(describe "lambda is let" (lambda ()
  (expect eqv? ((lambda (x) x) 42) 42)
  (expect eqv? ((lambda (x y) (+ x y)) 42 21) 63)
  (expect eqv? ((lambda (x)
                  ((lambda (y) (+ x y))
                   21)) 42) 63)
  (expect eqv? ((lambda (x)
                  ((lambda (x) x) 1))
                42) 1)
  (expect eqv? ((lambda (x)
                  ((lambda (y) y)
                   x)) 42) 42)
  (expect eqv? ((lambda (x)
                  ((lambda (x) x)
                   x)) 42) 42)
  (expect eqv? ((lambda (x)
                  ((lambda (x) x) 10)
                  x) 42) 42)
  (expect equal? ((lambda (x)
                    ((lambda (y) `(,x ,y))
                     10)) 42) '(42 10))))

(describe "lambda recursion" (lambda ()
  (expect eqv? (begin
                (define f (lambda (x)
                            (if (> x 0)
                                x
                                (f (+ x 1)))))
                (f 0)) 1)))

(describe "lambda variadic" (lambda ()
  (expect procedure? (lambda x 1))
  (expect eqv? ((lambda x 42)) 42)
  (expect eqv? ((lambda x (* 2 (car x))) 21) 42)
  (expect eqv? ((lambda x (* (car x) (car (cdr x))))
                3 14) 42)
  (expect eqv? (begin
                 (define mul (lambda x (* (car x) (car (cdr x)))))
                 (mul 3 14)) 42)
  (expect eqv? (begin
                 (define a 42)
                 ((lambda x a))) 42)
  (expect eqv? (begin
                 (define a 42)
                 ((lambda x ((lambda x a))))) 42)
  (expect eqv? (begin
                 (define a 42)
                 ((lambda a (car a)) 10)
                 a) 42)))

;; 4.1.5. Conditionals
(describe "if" (lambda ()
  (expect eqv? (if #t 1) 1)
  (expect eqv? (if #t 1 2) 1)
  (expect eqv? (if #f 1 2) 2)))

(describe "if composed" (lambda ()
  (expect eqv? (if (if #t 1 #f)
                   (if #t 3 4)
                   (if #t 5 6)) 3)
  (expect eqv? (if (if #f 1 #f)
                   (if #f 3 4)
                   (if #f 5 6)) 6)))

;; 4.1.6. Assignments
(describe "set!" (lambda ()
  (expect eqv? (begin
                 (define x 1)
                 (set! x 42)
                 x) 42)))

;; 5. Program structure
;; 5.2. Definitions
(describe "define variable" (lambda ()
  (expect eqv? (begin
                 (define x 42)
                 x) 42)
  (expect eqv? (begin
                 (define x (* -1 42))
                 x) -42)))

(describe "define function" (lambda ()
  (expect eqv? (begin
                 (define (f) 42)
                 (f)) 42)
  (expect eqv? (begin
                 (define (f x) (* -1 x))
                 (f 42)) -42)))

(describe "define function variadic" (lambda ()
  (expect eqv? (begin
                 (define (f . a) 42)
                 (f)) 42)
  (expect eqv? (begin
                 (define (f . a) (* -1 (car a)))
                 (f 42)) -42)))

(describe "define and lambda" (lambda ()
  (expect eqv? (begin
                 (define f (lambda () (g)))
                 (define g (lambda () 42))
                 (f)) 42)))

;; 6. Standard procedures
;; 6.1. Equivalence predicates
(describe "eq?" (lambda ()
  (expect eq? 'a 'a)
  (noexpect eq? (list 'a) (list 'a))
  (expect eq? '() '())
  (expect eq? car car)
  (let ((x '(a)))
    (expect eq? x x))
  (let ((x '()))
    (expect eq? x x))
  (let ((p (lambda (x) x)))
    (expect eq? p p))

  (expect eq? #t #t)
  (expect eq? #f #f)
  (noexpect eq? #t #f)
  (noexpect eq? () '(1))
  (noexpect eq? '(1) '(1))
  (noexpect eq? '(1 '(2)) '(1 '(2)))))

;; 6.3. Other data types
;; 6.3.1. Booleans
(describe "true/false" (lambda ()
  (expect-t #t)
  (expect-f #f)))

(describe "cons" (lambda ()
  (expect equal? '(1 . 2) (cons 1 2))))

(describe "car" (lambda ()
  (expect eqv? (car '(1 . 2)) 1)))

(describe "cdr" (lambda ()
  (expect eqv? (cdr '(1 . 2)) 2)))

;; 6.4. Control features
(describe "apply" (lambda ()
  (expect equal? (apply + '(42)) 42)
  (expect equal? (apply + 1 '(42)) 43)
  (expect equal? (apply + 1 2 '(42)) 45)
  (expect equal? (apply + 1 2 3 '(42)) 48)
  (expect equal? (apply + '(1 2 3 42)) 48)
  (expect equal? (apply + (apply + '(1 2 3)) '(42)) 48)
  (expect-t (apply equal? '((1) (1))))))

(describe "apply variadic" (lambda ()
  (expect-t (apply = 1 1 1 1 1 '(1 1 1 1 1)))))

(test-run)
