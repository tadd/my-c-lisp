(load "./lib.scm")

(describe "arithmetic literals" (lambda ()
  (expect eq? (+ 42 21) 63)
  (expect eq? (- 42 21) 21)
  (expect eq? (* 4 2) 8)
  (expect eq? (/ 4 2) 2)))

(describe "eval arithmetic expr" (lambda ()
  (expect eq? (+ (+ 40 2) 21) 63)
  (expect eq? (+ (- 40 4) (* 3 (/ 100 50))) 42)))

(describe "relop" (lambda ()
  (expect-t (= 42 42))
  (expect-t (= 0 0 0 0 0))

  (expect-f (= 42 0))
  (expect-f (= 0 0 0 0 42))

  (expect-t (< 2 4))
  (expect-t (< 2 3 4 5))

  (expect-f (< 2 0))
  (expect-f (< 2 3 4 4))

  (expect-t (<= 2 4))
  (expect-t (<= 2 3 4 4))

  (expect-f (<= 2 0))
  (expect-f (<= 2 3 4 3))

  (expect-t (> 3 2))
  (expect-t (> 4 3 2 1))

  (expect-f (> 0 1))
  (expect-f (> 4 3 2 2))

  (expect-t (>= 3 2))
  (expect-t (>= 4 3 2 2))

  (expect-f (>= 0 1))
  (expect-f (>= 4 3 2 3))))

(describe "modulo" (lambda ()
  (expect eq? (modulo 13 4) 1)
  (expect eq? (modulo -13 4) 3)
  (expect eq? (modulo 13 -4) -3)
  (expect eq? (modulo -13 -4) -1)))

(describe "true/false" (lambda ()
  (expect-t #t)
  (expect-f #f)))

(describe "if" (lambda ()
  (expect eq? (if #t 1) 1)
  (expect eq? (if #t 1 2) 1)
  (expect eq? (if #f 1 2) 2)))

(describe "if composed" (lambda ()
  (expect eq? (if (if #t 1 #f) (if #t 3 4) (if #t 5 6)) 3)
  (expect eq? (if (if #f 1 #f) (if #f 3 4) (if #f 5 6)) 6)))

(describe "list" (lambda ()
  (expect null? '())
  (expect null? (list))
  (let ((l (list 42 "foo")))
    ;; (expect pair? l)
    (expect eq? (length l) 2)
    (expect eq? (car l) 42)
    (expect equal? (car (cdr l)) "foo"))))

(describe "null" (lambda ()
  (expect-t (null? ()))
  (expect-t (null? (list)))
  (expect-f (null? '(1)))
  (expect-f (null? 1))))

(describe "reverse" (lambda ()
  (expect equal? () ())
  (expect equal? '(1) (reverse '(1)))
  (expect equal? '(2 1) (reverse '(1 2)))
  (expect equal? '(3 2 1) (reverse '(1 2 3)))))

(describe "cons etc" (lambda ()
  (expect equal? '(1 . 2) (cons 1 2))
  (expect eq? (car '(1 . 2)) 1)
  (expect eq? (cdr '(1 . 2)) 2)))

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

(describe "set!" (lambda ()
  (expect eq? 42 (begin
                   (define x 1)
                   (set! x 42)
                   x))))

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
                   (list x y)) '(42 10))))

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
                   (list x y)) '(42 10))))

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

(describe "apply variadic" (lambda ()
  (expect-t (= 1 1 1 1 1 1 1 1 1 1))))

(describe "lambda" (lambda ()
  ;; (expect procedure? (lambda () 1))
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

(describe "let is lambda" (lambda ()
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
                    ((lambda (y) (list x y))
                     10)) 42) '(42 10))))

(describe "lambda recursion" (lambda ()
  (expect eq? (begin
                (define f (lambda (x)
                            (if (> x 0)
                                x
                                (f (+ x 1)))))
                (f 0)) 1)))

(describe "lambda variadic" (lambda ()
  ;; (expect procedure? (lambda x 1))
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

(describe "begin" (lambda ()
  (expect eq? (begin 1 2 3) 3)))

(describe "cond" (lambda ()
  (expect eq? (cond (#f 1) (#t 2) (else 3)) 2)
  (expect eq? (cond (#f 1) (else 3)) 3)
  (expect eq? (cond (#f) (2)) 2)))

(describe "cputime" (lambda ()
  (let ((t (_cputime)))
  ;; (expect number? t)
    (expect > t 0)
    (expect-f (eq? t 0)))))

(describe "comment" (lambda ()
  (expect eq? 1 1 ; foo
              )
  (expect eq? 2 2 ;;bar
              )
  (expect equal? '(1 2) '(1 ;;; ?? ;;;
                          2))))

(describe "define and lambda" (lambda ()
  (expect eq? (begin
                (define f (lambda () (g)))
                (define g (lambda () 42))
                (f)) 42)))

(describe "call/cc" (lambda ()
  ;; Based on "yin-yang" in Kawa's test suite under the MIT license:
  ;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/r5rs_pitfall.scm
  (define r
    (let ((x ())
          (y 0)
          (id (lambda (x) x)))
      (call/cc
       (lambda (escape)
         (let* ((in ((lambda (foo)
                       (set! x (cons y x))
                       (if (= y 5)
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
  (expect equal? r '(5 4 3 2 1 0))))

(describe "eq" (lambda ()
  (expect-t (eq? #t #t))
  (expect-t (eq? #f #f))
  (expect-f (eq? #t #f))
  (expect-t (eq? 1 1))
  (expect-f (eq? 1 -1))
  (expect-t (eq? () ()))
  (expect-f (eq? () '(1)))
  (expect-t (let ((x '(1)))
              (eq? x x)))
  (expect-t (let ((p (lambda (x) x)))
              (eq? p p)))

  (expect-f (eq? '(1) '(1)))
  (expect-f (eq? '(1 '(2)) '(1 '(2))))))

(describe "equal" (lambda ()
  (expect-t (equal? #t #t))
  (expect-t (equal? #f #f))
  (expect-f (equal? #t #f))
  (expect-t (equal? 1 1))
  (expect-f (equal? 1 -1))
  (expect-t (equal? () ()))
  (expect-f (equal? () '(1)))
  (expect-t (let ((x '(1)))
              (equal? x x)))
  (expect-t (let ((p (lambda (x) x)))
              (equal? p p)))

  (expect-t (equal? '(1) '(1)))
  (expect-t (equal? '(1 '(2)) '(1 '(2))))

  (expect-t (equal? "abc" "abc"))
  (expect-t (equal? "\"" "\""))
  (expect-f (equal? "abc" "abd"))
  (expect-f (equal? "abc\"" "\""))))

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

(describe "map" (lambda ()
  (expect equal? (map car '((a b) (d e) (g h))) '(a d g))
  (expect equal? (map (lambda (n) (* n n)) '(1 2 3 4 5)) '(1 4 9 16 25))
  (expect equal? (map + '(1 2 3) '(4 5 6)) '(5 7 9))))

(describe "assq" (lambda ()
  (define alist '((10 . 1) (20 . 2) (30 . 3)))
  (expect equal? (assq 10 alist) '(10 . 1))
  (expect equal? (assq 20 alist) '(20 . 2))
  (expect equal? (assq 40 alist) #f)
  (expect equal?
          (assq '(1) '(((1) (2) (3))))
          #f)))

(describe "quote" (lambda ()
  (expect eq? (quote 10) 10)
  (expect eq? (quote ()) ())
  (expect equal? (quote (1 2 3)) '(1 2 3))
  (expect eq? (quote foooo) (quote foooo))
  (let ((l (quote (+ 1 2))))
     (expect equal? l (quote (+ 1 2)))
     (expect equal? l (list (quote +) 1 2)))))

(describe "quote2" (lambda ()
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

(test-run)
