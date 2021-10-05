;; My playground for learning me some Common Lisp

(format t "ello folks")


#|==================================================
Functions
==================================================|#

;; The obligatory Fibonacci function
(defun fib (n)
  "Return the nth Fibonacci number"
  (if (< n 2)
      n
      (+ (fib (- n 1))
	 (fib (- n 2)))))

;; Call using (fib 24)
;; Or call indirectly with (funcall #'fib (list 24))
;; OR with apply (apply #'fib (list 24))



;; Helloooo multiple function returns
(defun many-happy-returns (n)
  (values n (* n 2) (* n 3)))

#| (multiple-value-list (many-happy-returns 2)
=> (2 4 6)

(many-happy-returns 2)
=> 2
   4
   6

(multiple-variable-bind (one two three)
   (many-happy-returns 2)
   (list one two three))
=> (2 4 6)
|#


#|==================================================
Variables
==================================================|#


;; Defining variables
(let ((str "ello folks!"))
  (string-upcase str))
;; => ELLO FOLKS!

;; Defining multiple variables
(let ((x 1)
      (y 5))
  (+ y x))
;; => 6

;; Variable whose initial values depend on variables in the same form
(let* ((x 1)
       (y (+ x 1)))
  y)
;; => 2

;; Dynamic variables (kinda like globals but dynamically scoped)
;; defparameter requires initial value, defvar doesn't
;; defparameter variables are changed when code is reloaded with new initial values, defvar variables aren't
(defparameter *string* "Hey, I'm a global!")

(defun print-variable ()
  (print *string*))

(print-variable) ;; => "Hey, I'm a global!"

(let ((*string* "I am dynamic")) ;; Bind *string* to new value
  (print-variable))
;; => "I am dynamic"
(print-variable) ;; => "Hey, I'm a global!"



#|==================================================
Lists
==================================================|#

;; Simple definitiion
(list 1 2 3)

;; Accessing list elements
(car (list 1 2 3)) ;; => 1
(first (list 1 2 3)) ;; => 1
(second (list 1 2 3)) ;; => 2
(nth 11 (list 1 2 3 4 5 6 7 8 9 10 11 12 13)) ;; => 12 (remember, it's 0 indexed)

(cdr (list 1 2 3)) ;; => (2 3)


(defparameter the-list) ;; => THE-LIST
(print the-list) ;; => (1 2 3)
(setf (nth 1 the-list) 0) ;; => 0
(print the-list) ;; => (1 0 3)



;; Higher order functions on lists

(mapcar #'evenp (list 1 2 3 4 5 6))
;; => (NIL T NIL T NIL T)
(mapcar #'oddp (list 1 2 3 4 5 6))
;; => (T NIL T NIL T NIL)

(mapcar #'string-upcase (list "Howdy" "folks"))
;; => ("HOWDY" "FOLKS")

;; Define our own mapcar
(defun my-mapcar (func list)
    (if list
	(cons (funcall func (first list))
	    (my-mapcar func (rest list)))
	nil))
;; => MY-MAPCAR

(my-mapcar #'string-upcase (list "Howdy" "folks"))
;; => ("HOWDY" "FOLKS")


;; Reducing on lists

(reduce #'+ (list 1 2 3)) ;; => 6

(reduce #'(lambda (acc item)
	    (* acc item))
	(list 10 20 30))
;; => 6000

;; Get a better look at how reduce works under the hood
(reduce #'(lambda (acc item)
	(format t "ACC: ~A, ITEM: ~A~%" acc item)
	(* acc item))
    (list 1 2 3 4 5 6))
#|
ACC: 1, ITEM: 2
ACC: 2, ITEM: 3
ACC: 6, ITEM: 4
ACC: 24, ITEM: 5
ACC: 120, ITEM: 6
720
|#



;; Sorting

(sort (list 3 6 5 7 1 9 2 0) #'<) ;; => (0 1 2 3 5 6 7 9)


;; Destructuring

(defun destructure (list)
  (destructuring-bind (first second &rest others)
      list
    (format t "First: ~A~%" first)
    (format t "Second: ~A~%" second)
    (format t "Rest: ~A~%" others)))
#|
(destructure (list 1 2 3 4 5 6))
First: 1
Second: 2
Rest: (3 4 5 6)
NIL
|#


#|==================================================
Macros
==================================================|#

(defmacro while (condition &body body)
  `(loop while ,condition do (progn ,@body)))

#|
(while (some-condition)
  (do-something)
  (do-something-else))

Expands to:
(loop while (some-condition) do
  (progn
    (do-something)
    (do-something-else)))
|#




#|==================================================
Generic functions - an alternative to classes
==================================================|#

(defgeneric description (object)
  (:documentation "Return an object description"))

(defmethod description ((object integer))
  (format nil "The integer ~D" object))

(defmethod description ((object float))
  (format nil "The float ~3,3f" object))

;; (description 10) => "The integar 10"
;; (description 3.14) => "The float 3.140"




#|==================================================
Classes
==================================================|#

(defclass bicycle (vehicle)
  ((mass :reader bicycle-mass
	 :intiarg :mass
	 :type real
	 :documentation "The bike's mass"))
  (:documentation "A bicycle"))

(defclass canoe (vehicle)
  ((rowers :reader canoe-rowers
	   :initarg :rowers
	   :initform 0
	   :type (integer 0)
	   :documentation "The number of rowers"))
  (:documentation "A canoe"))

#|
(defparameter canoe (make-instance 'canoe
				   :speed 10
				   :rowers 6))
=> CANOE
(class-of canoe)
=> #<STANDARD-CLASS COMMON-LISP-USER::CANOE>

(canoe-rowers canoe)
=> 6

(vehicle-speed canoe)
=> 10

(describe 'canoe)
=> COMMON-LISP-USER::CANOE
  [symbol]

CANOE names the standard-class #<STANDARD-CLASS
				 COMMON-LISP-USER::CANOE>:
  Documentation:
    A canoe.
  Direct superclasses: VEHICLE
  No subclasses.
  Not yet finalized.
  Direct slots:
    ROWERS
      Type: (INTEGER 0)
      Initargs: :ROWERS
      Initform: 0
      Readers: CANOE-ROWERS
      Documentation:
       The number of rowers.
|#
