(load "./libtest.scm")

(describe "parsing comments" (lambda ()
  (expect eq? 1 1 ; foo
              )
  (expect eq? 2 2 ;;bar
              )
  (expect equal? '(1 2) '(1 ;;; ?? ;;;
                          2))))

;; 4.1. Primitive expression types
;; 4.1.2. Literal expressions
(describe "quote basic" (lambda ()
  (expect equal? '() (list))
  (expect equal? '(1) (list 1))
  (expect equal? '(1 2) (list 1 2))))

(describe "quote" (lambda ()
  (expect eq? '10 10)
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
  (expect eq? ((lambda () 42)) 42)
  (expect eq? ((lambda (x) (* 2 x)) 21) 42)
  (expect eq? ((lambda (x y) (* x y)) 3 14) 42)
  (expect eq? (begin
                (define mul (lambda (x y) (* x y)))
                (mul 3 14)) 42)
  (expect eq? (begin
                (define a 42)
                ((lambda () a))) 42)
  (expect eq? (begin
                (define a 42)
                ((lambda ()
                   ((lambda () a))))) 42)
  (expect eq? (begin
                (define a 42)
                ((lambda (a) a) 10)
                   a) 42)))

(describe "lambda2" (lambda ()
  (expect eq? (begin
                (define a 42)
                (define f (lambda () a))
                (define g (lambda () f))
                ((g))) 42)
  (expect eq? (begin
                (define a 42)
                (define f (lambda () a))
                (((lambda () f)))) 42)
  (expect eq? (begin
                (define a 42)
                (define f (lambda ()
                            (lambda () a)))
                (define g (f))
                (g)) 42)
  (expect eq? (begin
                (define a 42)
                (((lambda ()
                    (lambda () a))))) 42)
  (expect eq? (begin
                (define a 42)
                (define f (lambda () a))
                ((((lambda ()
                     (lambda () f)))))) 42)
  (expect eq? (((lambda ()
                  42
                  (lambda () 42)))) 42)
  (expect eq? ((((lambda ()
                   42
                   (lambda ()
                     (lambda () 42)))))) 42)))

(describe "lambda is let" (lambda ()
  (expect eq? ((lambda (x) x) 42) 42)
  (expect eq? ((lambda (x y) (+ x y)) 42 21) 63)
  (expect eq? ((lambda (x)
                 ((lambda (y) (+ x y))
                  21)) 42) 63)
  (expect eq? ((lambda (x)
                 ((lambda (x) x) 1))
               42) 1)
  (expect eq? ((lambda (x)
                    ((lambda (y) y)
                     x)) 42) 42)
  (expect eq? ((lambda (x)
                 ((lambda (x) x)
                  x)) 42) 42)
  (expect eq? ((lambda (x)
                 ((lambda (x) x) 10)
                 x) 42) 42)
  (expect equal? ((lambda (x)
                    ((lambda (y) `(,x ,y))
                     10)) 42) '(42 10))))

(describe "lambda recursion" (lambda ()
  (expect eq? (begin
                (define f (lambda (x)
                            (if (> x 0)
                                x
                                (f (+ x 1)))))
                (f 0)) 1)))

(describe "lambda variadic" (lambda ()
  (expect procedure? (lambda x 1))
  (expect eq? ((lambda x 42)) 42)
  (expect eq? ((lambda x (* 2 (car x))) 21) 42)
  (expect eq? ((lambda x (* (car x) (car (cdr x))))
               3 14) 42)
  (expect eq? (begin
                (define mul (lambda x (* (car x) (car (cdr x)))))
                (mul 3 14)) 42)
  (expect eq? (begin
                (define a 42)
                ((lambda x a))) 42)
  (expect eq? (begin
                (define a 42)
                ((lambda x ((lambda x a))))) 42)
  (expect eq? (begin
                (define a 42)
                ((lambda a (car a)) 10)
                a) 42)))

;; 4.1.5. Conditionals
(describe "if" (lambda ()
  (expect eq? (if #t 1) 1)
  (expect eq? (if #t 1 2) 1)
  (expect eq? (if #f 1 2) 2)))

(describe "if composed" (lambda ()
  (expect eq? (if (if #t 1 #f) (if #t 3 4) (if #t 5 6)) 3)
  (expect eq? (if (if #f 1 #f) (if #f 3 4) (if #f 5 6)) 6)))

;; 4.1.6. Assignments
;;define_special(e, "set!", builtin_set, 2);
(describe "set!" (lambda ()
  (expect eq? 42 (begin
                   (define x 1)
                   (set! x 42)
                   x))))

;; 4.2. Derived expression types
;; 4.2.1. Conditionals
(describe "cond" (lambda ()
  (expect eq? (cond (#f 1) (#t 2) (else 3)) 2)
  (expect eq? (cond (#f 1) (else 3)) 3)
  (expect eq? (cond (#f) (2)) 2)))

(describe "and" (lambda ()
  (expect-t (and))
  (expect-t (and #t))
  (expect eq? (and and) and)
  (expect eq? (and 1) 1)
  (expect eq? (and 1 "2" 'three) 'three)
  (expect-f (and #f))
  (expect-f (and 1 #f))
  (expect-f (and 1 'two #f))
  (define b #f)
  (define (f) (set! b #t) #f)
  (let* ((x (and #f (f))))
    (expect-f x)
    (expect-f b))))

(describe "or" (lambda ()
  (expect-f (or))
  (expect-t (or #t))
  (expect eq? (or or) or)
  (expect eq? 1 (or 1))
  (expect eq? (or 1 "2" 'three) 1)
  (expect-f (or #f))
  (expect-f (or #f #f))
  (expect eq? (or #f 1) 1)
  (expect eq? (or 'one 2 #f) 'one)
  (define b #t)
  (define (f) (set! b #f) #t)
  (let* ((x (or #t (f))))
    (expect-t x)
    (expect-t b))))

;; 4.2.2. Binding constructs
(describe "let" (lambda ()
  (expect eq? (let ((x 42)) x) 42)
  (expect eq? (let ((x 42) (y 21)) (+ x y)) 63)
  (expect eq? (let ((x 42))
                (let ((y 21))
                  (+ x y))) 63)
  (expect eq? (let ((x 42))
                  (let ((x 1))
                    x)) 1)
  (expect eq? (let ((x 42))
                (let ((y x))
                  y)) 42)
  (expect eq? (let ((x 42))
                   (let ((x x))
                     x)) 42)
  (expect eq? (let ((x 42))
                (let ((x 10))
                  x)
                x) 42)
  (expect equal? (let ((x 42) (y 10))
                   `(,x ,y)) '(42 10))))

(describe "let body define" (lambda ()
  (expect eq? (let ((x 42))
                (define x 2)
                x) 2)
  (expect eq? (begin
                (define x 1)
                (let ((x 42))
                  (define x 2)
                  x)) 2)
  (expect eq? (begin
                (define x 1)
                (let ()
                  (define x 2)
                  x)
                x) 1)))

(describe "let*" (lambda ()
  (expect equal? (let* ((x 42) (y 10))
                   `(,x ,y)) '(42 10))))

(describe "letrec" (lambda ()
  (define retval
    (letrec ((myeven?
              (lambda (n)
                (if (= n 0)
                    #t
                    (myodd? (- n 1)))))
             (myodd?
              (lambda (n)
                (if (= n 0)
                    #f
                    (myeven? (- n 1))))))
      (myeven? 88)))
  (expect-t retval)))

;; 4.2.3. Sequencing
(describe "begin" (lambda ()
  (expect eq? (begin 1 2 3) 3)))

;; 4.2.6. Quasiquotation
(describe "quasiquote basic" (lambda ()
  (expect equal? `() (list))
  (expect equal? `(1) (list 1))

  (expect equal? `(1 2) (list 1 2))
  (expect equal? `() (quasiquote ()))
  (expect equal? `(1) (quasiquote (1)))
  (expect equal? `(1 2) (quasiquote (1 2)))))

(describe "quasiquote" (lambda ()
  (expect eq? `10 10)
  (expect equal? `"abc" "abc")
  (expect equal? `#t #t)
  (expect eq? `() ())
  (expect equal? `(1 2 3) (list 1 2 3))
  (expect equal? `foooo (quasiquote foooo))
  (let ((l `(+ 1 2)))
     (expect equal? l (quasiquote (+ 1 2)))
     (expect equal? l (list (quasiquote +) 1 2)))))

(describe "quasiquote nested" (lambda ()
  (expect equal? ``1 '(quasiquote 1))
  (expect equal? ``(1) '(quasiquote (1)))
  (expect equal? `(`1) '((quasiquote 1)))
  (expect equal? ```1 '(quasiquote (quasiquote 1)))
  (expect equal? ```(1) '(quasiquote (quasiquote (1))))
  (expect equal? ``(`1) '(quasiquote ((quasiquote 1))))
  (expect equal? `(``1) '((quasiquote (quasiquote 1))))))

(describe "quasiquote unquote" (lambda ()
  (expect eq? `,() ())
  (expect eq? `,10 10)
  (expect eq? let `,let)
  (expect equal? `(,1 ,2 ,3) (list 1 2 3))
  (let ((x 40))
    (expect equal? `,(+ 2 x) 42)
    (expect equal? `(2 ,x) '(2 40))
    (expect equal? `(1 ,(/ x 2) ,x) '(1 20 40)))
  (expect equal? (let ((a 3))
                   `((1 2) ,a ,4 ,'five 6))
                 `((1 2) 3 4 five 6))
  (expect equal? (quasiquote (list (unquote (+ 1 2)) 4))
                 '(list 3 4))
  (expect equal? '(quasiquote (list (unquote (+ 1 2)) 4))
                 '`(list ,(+ 1 2) 4))))

(describe "quasiquote unquote nested" (lambda ()
  (expect equal? ``,1 '`,1)
  (expect equal? ``,,(+ 1 2) '`,3)

  (expect equal? `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
                 '(a `(b ,(+ 1 2) ,(foo 4 d) e) f))
  (expect equal? (let ((name1 'x)
                       (name2 'y))
                   `(a `(b ,,name1 ,',name2 d) e))
                 '(a `(b ,x ,'y d) e))))

(describe "quasiquote unquote-splicing" (lambda ()
  (expect equal? (let ((x '())) `(,@x)) '())
  (expect equal? (let ((x '(42))) `(,@x)) '(42))
  (expect equal? (let ((x '(1 2 3))) `(,@x)) '(1 2 3))
  (expect equal? `((foo ,(- 10 3)) ,@(cdr '(c)))
                 '((foo 7)))
  (expect equal? `(1 ,@(cdr '(2)) 3) '(1 3))
  (define (abs x)
    (if (< x 0) (- x) x))
  (expect equal? `(,@(map abs '(4 -5 6)))
                 '(4 5 6))
  (expect equal? `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
                 '(a 3 4 5 6 b))
  (define (sq x)
    (* x x))
  (expect equal? `(10 5 ,(sq 2) ,@(map sq '(4 3)) 8)
                 '(10 5 4 16 9 8))))

(describe "quasiquote improper lists" (lambda ()
  (expect equal? `(1 . 2) '(1 . 2))
  (expect equal? `(1 . ,(car '(2))) '(1 . 2))
  (expect equal? `((foo ,(- 10 3)) ,@(list 8) . ,(car '(9))) '((foo 7) 8 . 9))
  (expect equal? `((foo ,(- 10 3)) ,@(list 8) . ,(car '(cons))) '((foo 7) 8 . cons))
  (expect equal? `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(9))) '((foo 7) . 9))
  (expect equal? `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) '((foo 7) . cons))))

;; 5. Program structure
;; 5.2. Definitions
(describe "define variable" (lambda ()
  (expect eq? 42 (begin
                   (define x 42)
                   x))
  (expect eq? -42 (begin
                    (define x (* -1 42))
                    x))))

(describe "define function" (lambda ()
  (expect eq? 42 (begin
                   (define (f) 42)
                   (f)))
  (expect eq? -42 (begin
                    (define (f x) (* -1 x))
                    (f 42)))))

(describe "define function variadic" (lambda ()
  (expect eq? 42 (begin
                   (define (f . a) 42)
                   (f)))
  (expect eq? -42 (begin
                    (define (f . a) (* -1 (car a)))
                    (f 42)))))

(describe "define and lambda" (lambda ()
  (expect eq? (begin
                (define f (lambda () (g)))
                (define g (lambda () 42))
                (f)) 42)))

;; 6. Standard procedures
;; 6.1. Equivalence predicates
(describe "eqv?" (lambda ()
  (expect eqv? 'a 'a)
  (expect-f (eqv? 'a 'b))
  (expect eqv? 2 2)
  (expect eqv? '() '())
  (expect eqv? 100000000 100000000)
  (expect-f (eqv? (cons 1 2) (cons 1 2)))
  (expect-f (eqv? (lambda () 1)
                  (lambda () 2)))
  (expect-f (eqv? #f 'nil))
  (let ((p (lambda (x) x)))
    (expect eqv? p p))

  (expect eqv? #t #t)
  (expect eqv? #f #f)
  (let ((s "foo"))
    (expect eqv? s s))
  (expect eqv? eqv? eqv?)
  (expect-f (eqv? #t #f))
  (expect-f (eqv? #f #t))
  (expect-f (eqv? 1 2))
  (expect-f (eqv? '() '(1)))
  (expect-f (eqv? "a" "a"))
  (expect-f (eqv? #f 'nil))))

(describe "eqv? complicated" (lambda ()
  (define gen-counter
    (lambda ()
      (let ((n 0))
        (lambda () (set! n (+ n 1)) n))))
  (let ((g (gen-counter)))
    (expect eqv? g g))
  (expect-f (eqv? (gen-counter) (gen-counter)))

  (define gen-loser
    (lambda ()
      (let ((n 0))
        (lambda () (set! n (+ n 1)) 27))))
  (let ((g (gen-loser)))
    (expect eqv? g g))

  (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
           (g (lambda () (if (eqv? f g) 'g 'both))))
    (expect-f (eqv? f g)))))

(describe "eq?" (lambda ()
  (expect eq? 'a 'a)
  (expect-f (eq? (list 'a) (list 'a)))
  (expect eq? '() '())
  (expect eq? car car)
  (let ((x '(a)))
    (expect eq? x x))
  (let ((x '()))
    (expect eq? x x))
  (let ((p (lambda (x) x)))
    (expect eq? p p))

  (expect-t (eq? #t #t))
  (expect-t (eq? #f #f))
  (expect-f (eq? #t #f))
  (expect-t (eq? 1 1))
  (expect-f (eq? 1 -1))
  (expect-f (eq? () '(1)))
  (expect-f (eq? '(1) '(1)))
  (expect-f (eq? '(1 '(2)) '(1 '(2))))))

(describe "equal?" (lambda ()
  (expect equal? 'a 'a)
  (expect equal? '(a) '(a))
  (expect equal? '(a (b) c) '(a (b) c))
  (expect equal? "abc" "abc")
  (expect equal? 2 2)

  (expect-t (equal? #t #t))
  (expect-t (equal? #f #f))
  (expect-f (equal? #t #f))
  (expect-f (equal? 1 -1))
  (expect-t (equal? () ()))
  (expect-f (equal? () '(1)))
  (expect-t (let ((x '(1)))
              (equal? x x)))
  (expect-t (let ((p (lambda (x) x)))
              (equal? p p)))
  (expect-t (equal? "abc" "abc"))
  (expect-t (equal? "\"" "\""))
  (expect-f (equal? "abc" "abd"))
  (expect-f (equal? "abc\"" "\""))))

;; 6.2. Numbers
;; 6.2.5. Numerical operations
(describe "=" (lambda ()
  (expect-t (= 42 42))
  (expect-t (= 0 0 0 0 0))

  (expect-f (= 42 0))
  (expect-f (= 0 0 0 0 42))))

(describe "<" (lambda ()
  (expect-t (< 2 4))
  (expect-t (< 2 3 4 5))
  (expect-f (< 2 0))
  (expect-f (< 2 3 4 4))))

(describe ">" (lambda ()
  (expect-t (> 3 2))
  (expect-t (> 4 3 2 1))
  (expect-f (> 0 1))
  (expect-f (> 4 3 2 2))))

(describe "<=" (lambda ()
  (expect-t (<= 2 4))
  (expect-t (<= 2 3 4 4))
  (expect-f (<= 2 0))
  (expect-f (<= 2 3 4 3))))

(describe ">=" (lambda ()
  (expect-t (>= 3 2))
  (expect-t (>= 4 3 2 2))
  (expect-f (>= 0 1))
  (expect-f (>= 4 3 2 3))))

(describe "+" (lambda ()
  (expect eq? (+ 42 21) 63)))

(describe "-" (lambda ()
  (expect eq? (- 42 21) 21)))

(describe "*" (lambda ()
  (expect eq? (* 4 2) 8)))

(describe "/" (lambda ()
  (expect eq? (/ 4 2) 2)))

(describe "arithmetic" (lambda ()
  (expect eq? (+ (+ 40 2) 21) 63)
  (expect eq? (+ (- 40 4) (* 3 (/ 100 50))) 42)))

(describe "modulo" (lambda ()
  (expect eq? (modulo 13 4) 1)
  (expect eq? (modulo -13 4) 3)
  (expect eq? (modulo 13 -4) -3)
  (expect eq? (modulo -13 -4) -1)))

;; 6.3. Other data types
;; 6.3.1. Booleans
(describe "true/false" (lambda ()
  (expect-t #t)
  (expect-f #f)))

(describe "not" (lambda ()
  (expect not #f)
  (expect not (not #t))
  (expect not (not (not #f)))))

;; 6.3.2. Pairs and lists
(describe "pair?" (lambda ()
  (expect pair? '())
  (expect pair? '(1))
  (expect pair? '(1 . 2))
  (expect-f (pair? 1))
  (expect-f (pair? 'a))
  (expect-f (pair? #f))))

(describe "cons" (lambda ()
  (expect equal? '(1 . 2) (cons 1 2))))

(describe "car" (lambda ()
  (expect eq? (car '(1 . 2)) 1)))

(describe "cdr" (lambda ()
  (expect eq? (cdr '(1 . 2)) 2)))

(describe "cxxr" (lambda ()
  (define l '(1 2 3 4))
  (expect equal? (cadr l) 2)
  (expect equal? (caddr l) 3)
  (expect equal? (cadddr l) 4)))

(describe "null?" (lambda ()
  (expect-t (null? ()))
  (expect-t (null? (list)))
  (expect-f (null? '(1)))
  (expect-f (null? 1))))

(describe "list" (lambda ()
  (expect null? '())
  (expect null? (list))
  (let ((l '(42 "foo")))
    (expect pair? l)
    (expect eq? (length l) 2)
    (expect eq? (car l) 42)
    (expect equal? (car (cdr l)) "foo"))))

(describe "length" (lambda ()
  (expect eq? (length '()) 0)
  (expect eq? (length '(1)) 1)
  (expect eq? (length '(1 2 3 4)) 4)))

(describe "append" (lambda ()
  (expect null? (append))
  (expect equal? (append '(1))
          '(1))
  (expect equal? (append '(1) '(2))
          '(1 2))
  (expect equal? (append '(1) '(2 3))
          '(1 2 3))
  (expect equal? (append '(1 '(2)) '('(3)))
          '(1 '(2) '(3)))
  (expect equal? (append '(1 2) '(3 . 4))
          '(1 . (2 . (3 . 4))))
  (expect equal? (append '(1) '(2) '(3))
          '(1 2 3))))

(describe "reverse" (lambda ()
  (expect equal? () ())
  (expect equal? '(1) (reverse '(1)))
  (expect equal? '(2 1) (reverse '(1 2)))
  (expect equal? '(3 2 1) (reverse '(1 2 3)))))

(describe "assq" (lambda ()
  (define alist '((10 . 1) (20 . 2) (30 . 3)))
  (expect equal? (assq 10 alist) '(10 . 1))
  (expect equal? (assq 20 alist) '(20 . 2))
  (expect equal? (assq 40 alist) #f)
  (expect equal?
          (assq '(1) '(((1) (2) (3))))
          #f)))

;; 6.4. Control features
(describe "procedure?" (lambda ()
  (expect-t (procedure? car))
  (expect-f (procedure? 'car))
  (expect-t (procedure? (lambda (x) (* x x))))
  (expect-f (procedure? '(lambda (x) (* x x))))
  (expect-t (call/cc (lambda (c) (procedure? c))))))

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

(describe "map" (lambda ()
  (expect equal? (map car '((a b) (d e) (g h))) '(a d g))
  (expect equal? (map (lambda (n) (* n n)) '(1 2 3 4 5)) '(1 4 9 16 25))
  (expect equal? (map + '(1 2 3) '(4 5 6)) '(5 7 9))
  (expect equal? (map + '(1 2) '(4)) '(5))))

(describe "for-each" (lambda ()
  (let ((x '()))
    (for-each (lambda (l) (set! x (car l))) '((a b) (d e) (g h)))
    (expect eq? x 'g))
  (let ((x 0))
    (for-each (lambda (n) (set! x (* n n))) '(1 2 3 4 5))
    (expect eq? x 25))
  (let ((x 0))
    (for-each (lambda (a b) (set! x (+ a b))) '(1 2 3) '(4 5 6))
    (expect eq? x 9))
  (let ((x 0))
    (for-each (lambda (a b) (set! x (+ a b))) '(1 2) '(4))
    (expect eq? x 5))))

(describe "call/cc applicable in call/cc" (lambda ()
  (define (f)
    (call/cc call/cc)
    42)
  (expect eq? (f) 42)))

;; Above call/cc tests were from Kawa's test suite under the MIT license

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/r5rs_pitfall.scm
(describe "call/cc and lambda" (lambda ()
  (expect eq? (call/cc (lambda (c) (0 (c 1)))) 1)))

(describe "call/cc retlec" (lambda ()
  (define (f)
    (letrec ((x (call/cc list))
             (y (call/cc list)))
      (cond ((procedure? x) (x (pair? y)))
	    ((procedure? y) (y (pair? x))))
      (let ((x (car x))
            (y (car y)))
        (and (call/cc x) (call/cc y) (call/cc x)))))
  (expect-t (f))))

(describe "call/cc in-yo" (lambda ()
  (define r
    (let ((x ())
          (y 0)
          (id (lambda (x) x)))
      (call/cc
       (lambda (escape)
         (let* ((in ((lambda (foo)
                       (set! x (cons y x))
                       (if (= y 10)
                           (escape x)
                           (begin
                             (set! y 0)
                             foo)))
                     (call/cc id)))
                (yo ((lambda (foo)
                       (set! y (+ y 1))
                       foo)
                     (call/cc id))))
           (in yo))))))
  (expect equal? r '(10 9 8 7 6 5 4 3 2 1 0))))

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/sva40649.scm
(describe "call/cc NPE" (lambda ()
  (define (f1 f2) (f2))

  (define (fa x)
    (call/cc
     (lambda (k)
       (define (f3) x)
       (f1 f3))))

  (define (fb x)
    (call/cc
     (lambda (k)
       (define (f3) x)
       (f1 f3)
       (+ 10 x))))

  (define (fc x)
    (call/cc
     (lambda (k)
       (define (f3) x)
       (f1 f3)
       (if (> x 0)
           (k (+ 20 x)))
       (+ 10 x))))

  (expect eq? (fb 3) 13)
  (expect eq? (fc 3) 23)))
;; End of tests from Kawa

;; Local Extensions
(describe "cputime" (lambda ()
  (let ((t (_cputime)))
  ;; (expect number? t)
    (expect > t 0))))

;; (load "./test-callcc.scm")

(test-run)
