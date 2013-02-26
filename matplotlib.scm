(module matplotlib * 
(import chicken scheme srfi-1 foreign posix extras files)
(use posix foreigners shell irregex define-structure traversal)

;;;; misc

(define (##matplotlib#with-temporary-file _ f)
 (let* ((name (create-temporary-file))
        (result (f name)))
  (delete-file* name)
  result))

(define (##matplotlib#string*-append . objs)
 (cond ((null? objs) "")
       ((string? (car objs))
	(string-append (car objs) (apply ##matplotlib#string*-append (cdr objs))))
       ((number? (car objs))
	(string-append (number->string (car objs))
                       (apply ##matplotlib#string*-append (cdr objs))))
       (else (error "fuck-up"))))

;;;;

(define (python program data)
 (define (write-text-file lines pathname)
  (if (string=? pathname "-")
      (for-each (lambda (line) (display line) (newline)) lines)
      (call-with-output-file pathname
       (lambda (port)
        (for-each (lambda (line) (display line port) (newline port)) lines))))) 
 (define (lines string) (irregex-split "\n" string))
 (define (system-output command)
  (lines (execute (list command) capture: #t)))
 (##matplotlib#with-temporary-file
  "/tmp/code.py"
  (lambda (code-f)
   (##matplotlib#with-temporary-file
    "/tmp/data.py"
    (lambda (data-f)
     (write-text-file program code-f)
     (write-text-file data data-f)
     (system-output (string-append "python " code-f " " data-f)))))))

(define (##matplotlib#minimuml l) (minimum (map minimum l)))
(define (##matplotlib#maximuml l) (maximum (map maximum l)))

;;;;;;;;;

(define (with-temporary-object-file object f)
 (define (write-object-to-file object pathname)
  (cond ((string=? pathname "-") (pp object) (newline))
        (else (call-with-output-file pathname
               (lambda (port) (pp object port) (newline port))))))
 (##matplotlib#with-temporary-file
  "/tmp/data"
  (lambda (filename)
   (write-object-to-file object filename)
   (f filename))))

(define (mplot-py-includes)
 `("import matplotlib"
   "matplotlib.use('GtkAgg')"
   "import matplotlib.path as mpath"
   "import matplotlib.patches as mpatches"
   "import matplotlib.pyplot as plot"
   "import matplotlib.mlab as mlab"
   "from matplotlib import cm"
   "from mpl_toolkits.mplot3d import axes3d"
   "import numpy as np"
   "import os"
   "import sys"
   ,(string-append "sys.path.append('" (repository-path ) "')")
   "import sexp"))
(define (mplot-py-read-data filename)
 `(,(string-append
     "data = sexp.sexp.parseString(''.join(open('"
     filename
     "').read().splitlines())).asList()[0]")))
(define mplot-py-detach
 '("pid = os.fork()"
   "if(pid != 0):"
   "  exit(0)"))
(define mplot-py-onscreen `("plot.draw()" "plot.show()"))
(define (mplot-py-figure output) `(,(string-append "plot.savefig('" output "')")))

(define (mplot-run-onscreen plots)
 (plots (lambda (string)
	 (python `(,@(mplot-py-includes)
		   ,@string
		   ,@mplot-py-detach
		   ,@mplot-py-onscreen)
		 '()))))
(define (mplot-run-code plots)
 (plots (lambda (string)
	 (pp `(,@(mplot-py-includes)
	       ,@string
               ,@mplot-py-detach
	       ,@mplot-py-onscreen)))))
(define (mplot-run-figure output plots)
 (plots (lambda (string)
	 (python `(,@(mplot-py-includes)
		   ,@string
		   ,@(mplot-py-figure output))
		 '()))))

;; TODO, when imlib is up and running
;; (define (mplot-run-imlib plots)
;;  (##matplotlib#with-temporary-file
;;   "/tmp/matplotlib-imlib.png"
;;   (lambda (filename)
;;    (plots (lambda (string)
;; 	   (python `(,@(mplot-py-includes)
;; 		     ,@string
;; 		     ,@(mplot-py-figure filename))
;; 		   '())))
;;    (imlib-load-image-immediately! filename))))

(define (mplot-two a b)
 (lambda (run-function)
  (a (lambda (string1)
      (b (lambda (string2)
	  (run-function (append string1 string2))))))))

(define (mplot-return s)
 (let ((s (cond ((list? s) s)
		((string? s) (list s))
		(else (error "fuck-up")))))
  (lambda (run) (run s))))

(define (mplot a . b)
 (qreduce
  mplot-two
  (cons a b)
  (mplot-return "")))

;;; Convenience

(define (mplot-onscreen command . commands)
 (mplot-run-onscreen (apply mplot (cons command commands))))
(define (mplot-code command . commands)
 (mplot-run-code (apply mplot (cons command commands))))
(define (mplot-figure output command . commands)
 (mplot-run-figure output (apply mplot (cons command commands))))


(define (mplot-data-plot a b default . options)
 (mplot-return
  (string*options-append
   "p = plot.plot(data[" a "],data[" b "]"
   (mplot-quote-bare-option
    (mplot-option-add-default-bare default options))
   ")")))
(define (mplot-plot x y default . options)
 (mplot (mplot-read-data (list x y))
        (apply mplot-data-plot 0 1 default options)))
(define (mplot-show-plot x y default . options)
 (mplot-run-onscreen (apply mplot-plot x y default options)))

;;; Data, needs sexp.py

(define (mplot-read-data data)
 (lambda (f)
  (with-temporary-object-file
   data
   (lambda (filename)
    (f (mplot-py-read-data filename))))))

;;; Options

(define (string*options-append . objs)
 (cond ((null? objs) "")
       ((string? (car objs))
	(string-append (car objs) (apply string*options-append (cdr objs))))
       ((number? (car objs))
	(string-append (number->string (car objs)) (apply string*options-append (cdr objs))))
       ((list? (car objs))
	(string-append (mplot-options->string (car objs))
		       (apply string*options-append (cdr objs))))
       (else (error "fuck-up"))))

(define (mplot-option-keyword? option) (and (list? option) (= (length option) 2)))
(define (mplot-option-bare? option) (not (list? option)))
(define (mplot-option->string option)
 (cond ((mplot-option-keyword? option) (##matplotlib#string*-append (car option) "=" (cadr option)))
       ((mplot-option-bare? option) option)
       (else (error "fuck-up"))))

(define (mplot-options->string options)
 (foldl (lambda (a b) (##matplotlib#string*-append a "," (mplot-option->string b))) "" options))

(define (mplot-quote-bare-option options)
 (if (some mplot-option-bare? options)
     (cons
      (##matplotlib#string*-append "'" (mplot-option->string
                                        (car (remove-if-not mplot-option-bare? options)))
                                   "'")
      (remove-if mplot-option-bare? options))
     options))

(define (mplot-quote-bare-options options)
 (map (lambda (o)
       (if (mplot-option-bare? o)
	   (##matplotlib#string*-append "'" (mplot-option->string o) "'")
	   o))
      options))

(define (mplot-option-add-default-bare default options)
 (if (some mplot-option-bare? options)
     options
     (cons default options)))

;;; Plotting

(define (mplot-grid-lines)
 (mplot-return "ax.grid(True)"))

(define (mplot-date-line x y)
 (mplot
  (mplot-return "import random, datetime, pylab")
  (mplot-read-data (list x y))
  (mplot-return "data[0] = [datetime.timedelta(weeks=i) + datetime.datetime(2011,7,31) for i in data[0]]")
  (apply mplot-data-plot 0 1 "-" '())
  (mplot-return "from matplotlib.dates import DateFormatter, WeekdayLocator")
  (mplot-return "ax.xaxis.set_major_formatter(DateFormatter('%B'))")
  (mplot-return "ax.xaxis.set_minor_locator(WeekdayLocator(byweekday=6))")))

(define (mplot-x-label label)
 (mplot-return (##matplotlib#string*-append "plot.xlabel('" label "')")))
(define (mplot-y-label label)
 (mplot-return (##matplotlib#string*-append "plot.ylabel('" label "')")))
(define (mplot-z-label label)
 (mplot-return (##matplotlib#string*-append "plot.zlabel('" label "')")))

(define (mplot-data-histogram a bins . options)
 (mplot-return (string*options-append "plot.hist(data[" a "]," bins options ")")))
(define (mplot-histogram values bins . options)
 (mplot (mplot-read-data (list values))
        (apply mplot-data-histogram 0 bins options)))
(define (mplot-show-histogram values bins . options)
 (mplot-run-onscreen (apply mplot-histogram values bins options)))

(define (mplot-data-scatter a b . options) (apply mplot-data-plot a b "." options))
(define (mplot-scatter x y . options) (apply mplot-plot x y "." options))
(define (mplot-show-scatter x y . options) (apply mplot-show-plot x y "." options))

(define (mplot-line-style x y style . options) (apply mplot-plot x y style options))

(define (mplot-data-line a b . options) (apply mplot-data-plot a b "-" options))
(define (mplot-line x y . options) (apply mplot-plot x y "-" options))
(define (mplot-line-f f l . options) (apply mplot-plot l (map f l) "-" options))
(define (mplot-show-line x y . options) (apply mplot-show-plot x y "-" options))

;; TODO Split mplot-data out, will need to move some stuff into python
(define (mplot-contour xs ys zs . optional)
 (mplot (mplot-read-data (list xs ys zs))
        (mplot-return
         `(,@mplot-py-axis-3d-projection
           ,@(mplot-py-surface 0 1 2)
           ,@(mplot-py-contour 0 1 2 (##matplotlib#maximuml xs) (##matplotlib#maximuml ys) (##matplotlib#minimuml zs))
           ,@(mplot-py-axis-3d-labels xs ys zs)))))

;; TODO Split mplot-data out, will need to move some stuff into python
(define (mplot-projections xs ys zs . optional)
 (mplot (mplot-read-data (list xs ys zs))
        (mplot-return
         `(,@mplot-py-axis-3d-projection
           ,@(mplot-py-contour 0 1 2 (##matplotlib#maximuml xs) (##matplotlib#maximuml ys) (##matplotlib#minimuml zs))
           ,@(mplot-py-axis-3d-labels xs ys zs)))))

(define (mplot-contour-f f x-start x-end y-start y-end steps . options)
 (apply
  mplot-contour
  (map-n (lambda _ (map-linear (lambda (x) x) x-start x-end steps)) (+ steps 1))
  (map-linear (lambda (a) (map-n (lambda _ a) (+ steps 1))) y-start y-end steps)
  (map-linear
   (lambda (x) (map-linear (lambda (y) (f x y)) y-start y-end steps))
   x-start x-end steps)
  options))

(define (mplot-projections-f f x-start x-end y-start y-end steps . options)
 (apply
  mplot-projections
  (map-n (lambda _ (map-linear (lambda (x) x) x-start x-end steps)) (+ steps 1))
  (map-linear (lambda (a) (map-n (lambda _ a) (+ steps 1))) y-start y-end steps)
  (map-linear
   (lambda (x) (map-linear (lambda (y) (f x y)) y-start y-end steps))
   x-start x-end steps)
  options))

;; TODO Will go away when the above change happens
(define mplot-py-axis-3d-projection
 `("ax = plot.figure().gca(projection='3d')"))
(define (mplot-py-wireframe a b c)
 `(,(##matplotlib#string*-append
     "ax.plot_wireframe(np.array(data[" a "]), np.array(data[" b "]), np.array(data[" c "]), rstride=1, cstride=1)")))
(define (mplot-py-surface a b c)
 `(,(##matplotlib#string*-append
     "ax.plot_surface(np.array(data[" a "]), np.array(data[" b "]), np.array(data[" c "]), rstride=1, cstride=1, cmap=cm.Blues, alpha=0.3)")))
(define (mplot-py-contour a b c max-a max-b min-c)
 `(,(##matplotlib#string*-append
     "ax.contour(data[" a "], data[" b "], data[" c "], zdir='z', offset=" min-c ")")
   ,(##matplotlib#string*-append "ax.contour(data[" a "], data[" b "], data[" c "], zdir='x', offset=-" max-a ")")
   ,(##matplotlib#string*-append "ax.contour(data[" a "], data[" b "], data[" c "], zdir='y', offset=" max-b")")))
(define (mplot-py-axis-3d-labels xs ys zs)
 `("ax.set_xlabel('X')"
   ,(##matplotlib#string*-append "ax.set_xlim3d(" (##matplotlib#minimuml xs) ", " (##matplotlib#maximuml xs) ")")
   "ax.set_ylabel('Y')"
   ,(##matplotlib#string*-append "ax.set_ylim3d(" (##matplotlib#minimuml ys) ", " (##matplotlib#maximuml ys) ")")
   "ax.set_zlabel('Z')"
   ,(##matplotlib#string*-append "ax.set_zlim3d(" (##matplotlib#minimuml zs) ", " (##matplotlib#maximuml zs) ")")))


(define (mplot-subplot-data-bar a width offset . options)
 (mplot-return
  (string*options-append
   "ax.bar(np.arange(len(data["a"]))+" offset ", data["a"], "
   width options ")")))

(define (mplot-add-subplot r c f . options)
 (mplot-return
  (string*options-append
   "ax = plot.figure().add_subplot("
   r c f options
   ")")))

(define (mplot-subplot r c f . options)
 (mplot-return
  (string*options-append
   "ax = plot.subplot("
   r "," c "," f options
   ")")))

(define (mplot-bar-default-width nr) (/ 1 (+ nr 1)))

(define (mplot-subplot-bars-labelled values-list labels-list . options-list)
 (mplot (apply mplot-subplot-bars values-list options-list)
        (mplot-axis-xtick-spacing
         (length (first values-list))
         (/ (- 1 (mplot-bar-default-width (length values-list))) 2))
        (mplot-axis-xtick-labels labels-list)))

(define (mplot-subplot-bars values-list . options-list)
 (apply
  mplot
  (mplot-read-data values-list)
  (mplot-add-subplot 1 1 1)
  (let ((width (mplot-bar-default-width (length values-list))))
   (map-indexed (lambda (l i)
		 (apply mplot-subplot-data-bar i width (* width i)
			(if (null? options-list)
			    '()
			    (list-ref options-list i))))
		values-list))))

(define (mplot-add-title s . options)
 (mplot-return (string*options-append "plot.title('" s options "')")))

(define (mplot-axis-xtick-spacing nr width . options)
 (mplot-return (string*options-append "ax.set_xticks(np.arange(" nr ")+" width options ")")))

(define (string-tail string) (substring string 1 (string-length string)))

(define (mplot-axis-xtick-labels labels . options)
 (mplot-return (string*options-append
		"ax.set_xticklabels( (" (string-tail (mplot-options->string (mplot-quote-bare-options labels))) ") "
		options " )")))

(define (mplot-interpolate a b n)
 (mplot-return
  `(,(##matplotlib#string*-append
      "data[" b "] = mlab.stineman_interp(np.linspace(data[" a "][0],data[" a "][-1]," n "),data["a "],data[" b "],None)")
    ,(##matplotlib#string*-append "data[" a "] = np.linspace(data[" a "][0],data[" a "][-1]," n ")"))))

(define (mplot-plot-interpolated x y n . options)
 (mplot
  (mplot-read-data (list x y))
  (mplot-interpolate 0 1 n)
  (apply mplot-data-plot 0 1 "-" options)))

(define (mplot-square-axes)
 (mplot-return "plot.axes().set_aspect('equal')"))

(define (mplot-rotate-axes)
 (mplot-return
  (list "labels = plot.axes().get_xticklabels()"
	"for label in labels:"
	"    label.set_rotation(30)")))

(define (mplot-vertical-axes)
 (mplot-return
  (list "labels = plot.axes().get_xticklabels()"
	"for label in labels:"
	"    label.set_rotation('vertical')")))

(define (mplot-log-y)
 (mplot-return "plot.axes().set_yscale('log')"))

(define (mplot-xlim l u)
 (mplot-return (##matplotlib#string*-append "plot.xlim([" l "," u "])")))

(define (mplot-ylim l u)
 (mplot-return (##matplotlib#string*-append "plot.ylim([" l "," u "])")))

(define (mplot-map-linear f s e n)
 ;; TODO, This is a bug from what I can see
 ;; (define (unzip l)
 ;;  (if (null? l) '()
 ;;      (map-n (lambda (i) (map (lambda (e) (list-ref e i)) l))
 ;;       (length (first l)))))
 ;; (apply mplot-line (unzip (map-linear (lambda (x) (list x (f x))) s e n)))
 ;; Error: Arguments to inlined call of `matplotlib#mplot-line' 
 ;;        do not match parameter-list (x y . options)
 (let-values (((x y) (unzip2 (map-linear (lambda (x) (list x (f x))) s e n))))
  (mplot-line x y)))

(define (mplot-line1 y . options) 
 (apply mplot-line (enumerate (length y)) y options))

(define (mplot-density-bandwidth l h . s/e)
 (define (sum l . n)
  (cond ((null? n) (qreduce + l 0))
        ((and (= (length n) 1) (procedure? l) (number? (car n)))
         (let ((f l) (n (first n)))
          (let loop ((n (- n 1)) (c 0))
           (if (negative? n) c (loop (- n 1) (+ c (f n)))))))
        ((and (procedure? l) (>= (length n) 1))
         (apply qmap-reduce + 0 l n))
        (else (error "fuck-up"))))
 (define pi (acos -1.0))
 (define (sqr x) (* x x))
 (define (gaussian-pdf x mu Sigma)
  (/ (exp (- 0
             (/ (sqr (- x mu))
                (* 2 Sigma))))
     (sqrt (* 2 (* pi Sigma)))))
 (let ((n (length l)))
  (define (f x) (/ (sum (lambda (xi) 
                  (gaussian-pdf x xi h)
                  ;; (gaussian-pdf (/ (- x xi) h))
                  ) l) (* n h)))
  (mplot-map-linear f
                    (if (null? s/e)
                        (minimum l)
                        (car s/e))
                    (if (null? s/e)
                        (maximum l)
                        (cadr s/e))
                    100)))

(define (mplot-matshow data . options)
 (mplot (mplot-read-data data)
        (mplot-return (string*options-append "ax.matshow(data" options ")"))))

(define (mplot-density l . s/e)
 (define (sqr x) (* x x))
 (define (list-mean p) 
  (/ (qreduce + p 0) (length p)))
 (define (list-variance s)
  (let ((mu (list-mean s)))
   (/ (qreduce + (map (lambda (s) (sqr (- s mu))) s) 0) (length s))))
 (apply
  mplot-density-bandwidth
  l
  (/ (expt (/ (* 4 (expt (list-variance l) 5)) 
              (* 3 (length l))) (/ 1 5)) 
     ;; A hack, the optimal bandwidth for normal distribution tends to over-smooth
     3)
  s/e))
)
