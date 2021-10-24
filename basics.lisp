;; My playground for learning me some Common Lisp

(format t "ello folks")


#|==================================================
Syntax info
==================================================|#
#|
#' can be followed by a function name to indicate to the compiler the function can be byte compiled.
(#'function-name) is equivalent to ('function-name)
|#
(defun my-add (a b)
  (+ a b))
(eq 'my-add #'my-add) ; => T

(lambda (x) (* x x))
(function (lambda (x) (* x x)))
#'(lambda (x) (* x x))


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
(many-happy-returns 2)

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


(defvar *x*) ; Unbound variable
(defparameter *x* 15) ; 15 assigned to X
(defvar *x* 10) ; Does nothing as X is already bound

;; Global constant - cannot be used as function parameters or rebound to another value
(defconstant +my-constant+ 20)


;; SETF is general purpose assignment macro. (setf place value)
(setf x 10)    ; x set to 10
(setf x 1 y 2) ; x set to 1, y set to 2
(incf x)       ; Same as (setf x (+ x 1))
(decf x)       ; Same as (setf x (- x 1))
(incf x 10)    ; Same as (setf x (+ x 10))



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


#|==========================================
READ
==========================================|#

; (defparameter my-variable nil)

(setf my-variable (read-line))
(setf my-variable (read))
(setf my-variable (read-from-string))


#|==========================================
PRINT
Print string directives (all preceded by ~)
~% - new line (TERPRI)
~A - insert string version of Lisp object (PRINC)
~S - print for READ function (PRIN1)
==========================================|#

; (format destination control-string optional-arguments*)

(format t "Dear ~A, ~% How are  you?" "Bob")
#|
Dear Bob,
 How are you?
|#

(format t "Dear ~S, How are you?" "Bob")
#|
Dear "Bob", How are you?
|#

(format nil "~A ~A" "Number is:" (+ 1 2))
#|
"Number us: 3" (a string)
|#



#|==========================================================
String functions
CONCATENATE - obvs
LENGTH      - obvs
SUBSEQ      - extract a substring
SEARCH      - search within a string, returning the index
==========================================================|#

(concatenate 'string "Hello, " "world" ". Today is a good day.")
;; => "Hello, world. Today is a good day."

(length "Comman") ; => 6
(search "term" "a term is a period of time") ; => 2
(subseq "Common Lisp" 7 11) ; => "Lisp"



#|==========================================================
Blocks
==========================================================|#

;; PROGN evaluates a sequence of forms in order and returns the value of the last form as the PROGN value
(progn
  (print "Hello")
  (print "World")
  (+ 5 5))
;; => 10

;; BLOCK is similar to PROGN but is a named block, and has a mechanism for out-of-order exit with RETURN-FORM.
(block my-block
  (print "We see this")
  (return-from my-block 10)
  (print "This will never be reached :("))
;; => 10



#|============================================================
Boolean & Logic
============================================================|#

(if 55 (print "True") (print "False")) ; => True"

(and t (+ 1 2) (* 1 5)) ; => 5
(or nil (+ 1 2) (* 1 5)) ; => 3

(eq 5 5) ; => T
(eq "5" "5") ; => NIL
(equal)

(string-equal "str" "str") ; => T
(string-equal "str" "STR") ; => T


(if (equal 5 (+ 1 4))
    (print "This is true")
    (print "This is false")) ; => "This is true"


(​when​ (equal 5 (+ 1 4))
      (print "Print if statement is true")
      (print "Print this also"))
#|
=> "Print if statement is true"
=> "Print this also"
|#

(unless (equal 3 (+ 1 4))
  (print "Only print if condition is false")
  (print "Print this also"))
#|
=> "Only print if condition is false"
=> "Print this also"
|#

(cond ((equal 5 3) (print "This will not print"))
      ((equal 5 5) (print "This will print"))
      ((equal 5 5)
       (print "This will not print as the")
       (print "form exited at first true"))) ; => "This will print"

(case (read)
  ((1 3 5 7 (* 3 3)) "Odd")
  (0
   (print "Zero")
   (print "Number"))
  (otherwise "Not an odd number < 10"))

#|
=> 9 RET
=> "Not an odd number < 10"
=> 0 RET

=> "Zero"
=> "Number"
|#



#|==================================================
Local functions
==================================================|#

(labels ((first-function (x) (+ x x))
	   (second-function (y) (* y y))
	   (third-function (z) (first-function z)))
    (+ (first-function 1)
       (second-function 2)
       (third-function 3)))
; => 12
