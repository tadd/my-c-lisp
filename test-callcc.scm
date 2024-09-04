;; All of above call/cc tests were from Kawa's test suite under the MIT license

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/r5rs_pitfall.scm
;; Cause: maybe just a bug?
(describe "call/cc defines in retlec body" (lambda ()
  (define (f)
    (let ((cont #f))
      (letrec ((x (call/cc (lambda (c) (set! cont c) 0)))
               (y (call/cc (lambda (c) (set! cont c) 0))))
        (if cont
            (let ((c cont))
              (set! cont #f)
              (set! x 1)
              (set! y 1)
              (c 0))
            (+ x y)))))
  (expect eq? (f) 0)))

;; Cause: needs cadr
(describe "call/cc retlec and set!" (lambda ()
  (define (f)
    (letrec ((x (call/cc
		 (lambda (c)
		   (list #t c)))))
      (if (car x)
	  ((cadr x) (list #f (lambda () x)))
	  (eq? x ((cadr x))))))
  (expect-t (f))))

;; Cause: case
(describe "call/cc each other" (lambda ()
  (define r #f)
  (define a #f)
  (define b #f)
  (define c #f)
  (define i 0)
  (define (f1)
    (let ()
      (set! r (+ 1 (+ 2 (+ 3 (call/cc (lambda (k) (set! a k) 4))))
                 (+ 5 (+ 6 (call/cc (lambda (k) (set! b k) 7))))))
      (if (not c)
          (set! c a))
      (set! i (+ i 1))
      (case i
        ((1) (a 5))
        ((2) (b 8))
        ((3) (a 6))
        ((4) (c 4)))
      r))
  (define (f2)
    (let ()
      (set! r (+ 1 (+ 2 (+ 3 (call/cc (lambda (k) (set! a k) 4))))
                 (+ 5 (+ 6 (call/cc (lambda (k) (set! b k) 7))))))
      (if (not c)
          (set! c a))
      (set! i (+ i 1))
      (case i
        ((1) (b 8))
        ((2) (a 5))
        ((3) (b 7))
        ((4) (c 4)))
      r))
  (expect eq? (f1) 28)
  (expect eq? (f2) 28)))

;; Cause: case
(describe "call/cc lazy callframe" (lambda ()
  (define (f)
    (let ((k1 #f)
          (k2 #f)
          (k3 #f)
          (state 0))
      (define (identity x) x)
      (define (fn)
        ((identity (if (= state 0)
                       (call/cc (lambda (k) (set! k1 k) +))
                       +))
         (identity (if (= state 0)
                       (call/cc (lambda (k) (set! k2 k) 1))
                       1))
         (identity (if (= state 0)
                       (call/cc (lambda (k) (set! k3 k) 2))
                       2))))
      (define (check states)
        (set! state 0)
        (let* ((res '())
               (r (fn)))
          (set! res (cons r res))
          (if (null? states)
              res
              (begin (set! state (car states))
                     (set! states (cdr states))
                     (case state
                       ((1) (k3 4))
                       ((2) (k2 2))
                       ((3) (k1 -)))))))
      (map check '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))))
  (expect equal? (f) '((-1 4 5 3)
                       (4 -1 5 3)
                       (-1 5 4 3)
                       (5 -1 4 3)
                       (4 5 -1 3)
                       (5 4 -1 3)))))

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/sva35362.scm
;; Cause: named let
(describe "call/cc unused" (lambda ()
  (define (f)
    (call/cc
     (lambda (return)
       (let l ()
         (l))))
    #t)
  (expect-t (f))))

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/unreach1.scm
;; Cause: named let
(describe "call/cc unreach 1" (lambda ()
  (define (f)
    (call/cc
     (lambda (return)
       (let l ()
         (return #f)
         (l))))
    #t)
  (expect-t (f))))

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/unreach2.scm
;; Cause: named let
(describe "call/cc unreach 2" (lambda ()
  (define (f)
    (call/cc
     (lambda (return)
       (let ((a 1))
         (let loop ((a a))
           (let ((finish (lambda (a) (return #f))))
             (finish a)
             (let ((a 2))
               (loop a))))))))
  (expect-f (f))))
