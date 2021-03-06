#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (rnrs eval)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (xitomatl conditionals))

(define-syntax check-SV
  (syntax-rules ()
    ((_ expr)
     (check (catch ex ((else (syntax-violation? ex)))
              (eval 'expr (environment '(rnrs) '(xitomatl conditionals)))
              'unexpected-return)
            => #T))))

;;;; aif

(check (aif x (+ 1 2) 
         (* x x) 
         (assert #F))
       => 9)
(check (aif x (string? 'sym) 
         (assert #F) 
         (list x))
       => '(#F))
(check (aif x number? (+ 1 2)
         (* x x) 
         (assert #F))
       => 9)
(check (aif x integer? (+ 1.1 2)
         (assert #F) 
         (- x))
       => -3.1)
(let ((a 0) (b 0) (c 0) (d 0))
  (check (aif x (begin (set! a (+ 1 a)) integer?) (begin (set! b (+ 1 b)) (+ 1.1 2))
              (begin (set! c (+ 1 c)) 'bad) 
              (begin (set! d (+ 1 d)) (- x)))
         
         => -3.1)
  (check (list a b c d) => '(1 1 0 1)))
(check-SV (aif "oops" 'foo 'true 'false))

;;;; xor

(check (xor) => #F)
(check (xor (number? 1)) => #T)
(check (xor (null? 1)) => #F)
(check (xor (string->symbol "foo")) => 'foo)
(check (xor (string? "a") (symbol? 1)) => #T)
(check (xor (string? 1) (symbol? 'a)) => #T)
(check (xor (string? 1) (symbol? 2)) => #F)
(check (xor (pair? '(a)) (list? '(b))) => #F)
(check (xor (- 42) (not 42)) => -42)
(check (xor (null? 1) (/ 42)) => 1/42)
(check (xor (integer? 1.2) (positive? -2) (exact? 3)) => #T)
(check (xor (integer? 1.2) (positive? 2) (exact? 3.4)) => #T)
(check (xor (integer? 1) (positive? -2) (exact? 3.4)) => #T)
(check (xor (integer? 1.2) (positive? -2) (exact? 3.4)) => #F)
(check (xor (integer? 1.2) (positive? 2) (exact? 3)) => #F)
(check (xor (integer? 1) (positive? -2) (exact? 3)) => #F)
(check (xor (integer? 1) (positive? 2) (exact? 3.4)) => #F)
(check (xor (integer? 1) (positive? 2) (exact? 3)) => #F)
(check (xor "foo" (not 'foo) (eq? 'a 'b)) => "foo")
(check (xor (not 'foo) (+ 1 2) (eq? 'a 'b)) => 3)
(check (xor (not 'foo) (eq? 'a 'b) (- 1 2)) => -1)
(let ((x '()))
  (check (xor (begin (set! x (cons 'a x)) #F)
              (begin (set! x (cons 'b x)) #F)
              (begin (set! x (cons 'c x)) #F)
              (begin (set! x (cons 'd x)) #F))
         => #F)
  (check x => '(d c b a)))
(let ((x '()))
  (check (xor (begin (set! x (cons 'a x)) 'R)
              (begin (set! x (cons 'b x)) #F)
              (begin (set! x (cons 'c x)) #F)
              (begin (set! x (cons 'd x)) #F))
         => 'R)
  (check x => '(d c b a)))
(let ((x '()))
  (check (xor (begin (set! x (cons 'a x)) #T)
              (begin (set! x (cons 'b x)) #F)
              (begin (set! x (cons 'c x)) #T)
              (begin (set! x (cons 'd x)) #F))
         => #F)
  (check x => '(c b a)))
(let-syntax ((macro
              (let ((count 0))
                (lambda (stx)
                  (syntax-case stx ()
                    ((_) (begin (set! count (+ 1 count)) #''foo))
                    ((_ _) count))))))
  (check (xor #F (macro) #F) => 'foo)
  (check (macro 'count) => 1))
(check-SV xor)


(check-report)
