(use posix shell irregex
     define-structure traversal matplotlib random-bsd)

(define pi (acos -1.0))
(define two-pi (* 2.0 pi))
(define minus-pi (- pi))
(define two-pi/360 (/ two-pi 360.0))
(define three-sixty/two-pi (/ 360.0 two-pi))
(define (degrees->radians angle) (* two-pi/360 angle))
(define (radians->degrees angle) (* three-sixty/two-pi angle))

(mplot-run-onscreen
 (mplot-histogram (map-n (lambda (a) (random-integer 10)) 100) 4))

(mplot-run-onscreen
 (mplot-histogram (map-n (lambda (a) (random-integer 10)) 100)
                  4 '("cumulative" "True")))

(mplot-run-onscreen
 (mplot-scatter '(1 2 3 4) '(3 4 5 6)))

(mplot-run-onscreen
 (mplot-contour-f (lambda (a b) (+ (sin (degrees->radians a))
                              (cos (degrees->radians b))))
                  -100 100 -200 200 10))

(mplot-run-onscreen
 (mplot (mplot-subplot-bars-labelled
         (list '(40 50 20 50) '(10 20 30 40) '(40 50 20 50))
         (list "a" "b" "c" "d"))
        (mplot-add-title "Wee a title")
        (mplot-x-label "X")
        (mplot-y-label "Y")))
;; http://en.wikipedia.org/wiki/File:Comparison_of_1D_histogram_and_KDE.png
(mplot-run-onscreen 
 (mplot-density-bandwidth '(-2.1 -1.3 -0.4 1.9 5.1 6.2) 2.25 -6 11))
