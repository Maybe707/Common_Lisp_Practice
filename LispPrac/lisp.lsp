(defun run()
    (compile-file "lisp.lsp")
    (load "lisp.fasl"))

(defmacro setq2 (v1 v2 e)
    (list 'progn (list 'setq v1 e) (list 'setq v2 e)))

(defun setq2-function (v1 v2 e)
    (list 'progn (list 'setq v1 e) (list 'setq v2 e)))

(defmacro setq-reversible (e1 e2 direction)
    (case direction
        (:normal (list 'setq e1 e2))
        (:backward (list 'setq e2 e1))
        (t (error "Unknown direction: ~a" direction))))

(defmacro setq3 (v1 v2 e)
    `(progn (setq ,v1 ,e) (setq ,@v2 ,@e)))

(defmacro explain-exp (exp)
    `(format t "~S = ~S" ',exp ,exp))

(defgeneric description (object)
    (:documentation "Return a description of an object"))

(defmethod description ((object integer))
    (format nil "The integer ~D" object))

(defmethod description ((object float))
    (format nil "The float ~3,3f" object))
    
(defclass vehicle ()
    ((speed :accessor vehicle-speed
         :initarg :speed
         :type real
         :documentation "The vehicle's current speed."))
    (:documentation "The base class of vehicle."))

(defclass bicycle (vehicle)
  ((mass :reader bicycle-mass
         :initarg :mass
         :type real
         :documentation "The bike's mass."))
  (:documentation "A bicycle."))

(defclass canoe (vehicle)
  ((rowers :reader canoe-rowers
           :initarg :rowers
           :initform 0
           :type (integer 0)
           :documentation "The number of rowers."))
  (:documentation "A canoe."))

(defun fib (n)
    "Return the nth Fibonacci number."
    (if (< n 2)
        n
        (+ (fib (- n 1))
           (fib (- n 2)))))

(defun func4 (x y)
    (+ x y))

(defun func5 (func4 n m)
    (funcall func4 n m))

(defun funcList1 (x)
    (apply #'+ x (List 1 2 4 5 6)))

(defun funcList2 (x y)
    (apply #'+ x y)) ;; Give to parameter 'y' List with various arguments

(defun funcVar ()
    (let ((str "Hello, World!"))
        (string-upcase str)))

(defun funcMultiVar ()
    (let ((x 1)(y 5)(z 10))
        (+ z (+ x y))))

(defun underVar ()
    (let* ((x 1)
              (y (+ x 1))
              (z (+ x y)))
        z))

(defvar *var* (funcMultiVar))
(defvar *variable*)

(setq *variable* 100)

(defparameter *string* "I'm global")

(defun print-variable ()
    (print *string*))

(defun newDynemicVar ()
    (let ((*string* "I have dynamic extent"))
    (print-variable)))
    ;; The old value is restored

(defun many3 (n)
    (values n (* n 2) (* n 3)))

(defun funcMultList (x)
    (multiple-value-list (many3 x)))

(defun funcNthList (n x)
    (nth-value n (many3 x)))

(defun funcBindList (x)
    (multiple-value-bind (first second third)
        (many3 x)
        (values first second third)))

(defun mapFunc()
    (mapcar #'evenp (list 1 2 3 4 5 6)))

(defun my-map (function list)
    (if list
        (cons (funcall function (first list))
                  (my-map function (rest list)))
                  nil))

(defun funcReduce ()
    (reduce #'+ (list 1 2 3)))

(defun funcReduce2 ()
    (reduce #'(lambda (a b)
             (* a b))
        (list 10 20 30)))

(defun funcReduceFormat ()
    (reduce #'(lambda (a b)
                  (format t "A: ~A, B: ~A~%" a b)
                  (* a b))
        (list 1 2 3 4 5 6)))

(defun funcSort ()
    (sort (list 9 2 4 7 3 0 8) #'<))

(defun fooKey (&key x y) (cons x y))

(defun destructure (list)
  (destructuring-bind (first second &rest others)
    list
    (format t "First: ~A~%" first)
    (format t "Second: ~A~%" second)
    (format t "Rest: ~A~%" others)))

(defun openFileFun ()
    (with-open-file (stream (merge-pathnames #p"data.txt"
                                         (user-homedir-pathname))
                        :direction :output    ;; Write to disk
                        :if-exists :supersede ;; Overwrite the file
                        :if-does-not-exist :create)
        (dotimes (i 100)
            ;; Write random numbers to the file
            (format stream "~3,3f~%" (random 100)))))

(defun readFileFun ()
    (uiop:read-file-string (merge-pathnames #p"data.txt"
                                                 (user-homedir-pathname))))

(defgeneric description (object)
  (:documentation "Return a description of an object."))

(defmethod description ((object integer))
  (format nil "The integer ~D" object))

(defmethod description ((object float))
  (format nil "The float ~3,3f" object))

(defmethod description ((object string))
    (format nil "The string: ~A" object))

(defmethod description ((object character))
    (format nil "The char: ~c" object))

(defclass vehicle ()
  ((speed :accessor vehicle-speed
          :initarg :speed
          :type real
          :documentation "The vehicle's current speed."))
  (:documentation "The base class of vehicles."))

(defclass bicycle (vehicle)
  ((mass :reader bicycle-mass
         :initarg :mass
         :type real
         :documentation "The bike's mass."))
  (:documentation "A bicycle."))

(defclass canoe (vehicle)
  ((rowers :reader canoe-rowers
           :initarg :rowers
           :initform 0
           :type (integer 0)
           :documentation "The number of rowers."))
  (:documentation "A canoe."))

(defparameter canoe (make-instance 'canoe
                                            :speed 10
                                            :rowers 6))

(defclass person ()
  ((name
    :initarg :name
    :accessor name)
   (lisper
    :initform nil
    :initarg :lisper   
    :accessor lisper)))

(defvar p1 (make-instance 'person :name "me" ))

(defclass child (person)
  ((can-walk-p
     :accessor can-walk-p
     :initform t
     :initarg :can-walk-p)))
;; #<STANDARD-CLASS CHILD>

(defclass hren (child)
    ((gear
         :accessor gear
         :initform t
         :initarg :gear)))

(defclass quakeTop1 (hren)
    ((gear2
         :accessor gear2
         :initform t
         :initarg :gear2)))

(defvar c1 (make-instance 'child :name "Quake"))

(defvar q1 (make-instance 'quakeTop1 :name "QuakeTop"))

;; (defvar h1 (make-instance 'hren :name "Urod"))

(defun make-person (name &key lisper)
  (make-instance 'person :name name :lisper lisper))

(defclass point ()
  (x y z))

(defvar pt (make-instance 'point))

(setf (slot-value pt 'x) 1)

(defun funcWithSlots1 ()
    (with-slots (name lisper)
    p1
    (format t "got ~a, ~a~&" name lisper)))

(defun funcWithSlots2 ()
    (with-slots ((n name)
             (l lisper))
    p1
  (format t "got ~a, ~a~&" n l)))

(defun funcAccessor ()
    (with-accessors ((name        name)
                 (lisper lisper))
            p1
          (format t "name: ~a, lisper: ~a" name lisper)))

(defmethod print-object ((obj person) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((name name)
                         (lisper lisper))
            obj
          (format stream "~a, lisper: ~a" name lisper))))

(defmethod print-object ((obj person) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "~a, lisper: ~a" (name obj) (lisper obj))))

(defclass android ()
    ((processor
        :initform 10
        :accessor processor
        :initarg  :processor)))

(defvar a1 (make-instance 'android))

(defmethod print-object ((obj android) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "~a" (processor obj))))

;(defmethod print-object ((obj android) stream)
;  (print-unreadable-object (obj stream :type t :identity t))) ;For reference, the following reproduces the default behaviour

(setf c1 (make-instance 'child :name "Alice"))

(setf h1 (make-instance 'hren :name "Vasya"))

(fmakunbound 'greet)

(defmethod greet (obj)
  (format t "Are you a person ? You are a ~a.~&" (type-of obj)))
;; style-warning: Implicitly creating new generic function common-lisp-user::greet.
;; #<STANDARD-METHOD GREET (t) {1008EE4603}>

(defgeneric greet (obj)
  (:documentation "say hello"))
;; STYLE-WARNING: redefining COMMON-LISP-USER::GREET in DEFGENERIC
;; #<STANDARD-GENERIC-FUNCTION GREET (2)>

(defmethod greet ((obj person))
  (format t "Hello ~a !~&" (name obj)))
;; #<STANDARD-METHOD GREET (PERSON) {1007C26743}>

(defmethod greet ((obj child))
  (format t "ur so cute~&"))

(defmethod greet ((obj hren))
  (format t "ur so shity~&"))

(defmethod greet :before ((obj person))
  (format t "-- before person~&"))
;;#<STANDARD-METHOD GREET :BEFORE (PERSON) {100C94A013}>

(defmethod greet :before ((obj child))
  (format t "-- before child~&"))
;; #<STANDARD-METHOD GREET :BEFORE (CHILD) {100AD32A43}>

(defmethod greet :before ((obj hren))
  (format t "-- before hren~&"))
;; #<STANDARD-METHOD GREET :BEFORE (CHILD) {100AD32A43}>

(defmethod greet :after ((obj person))
  (format t "-- after person~&"))
;; #<STANDARD-METHOD GREET :AFTER (PERSON) {100CA2E1A3}>

(defmethod greet :after ((obj child))
  (format t "-- after child~&"))
;; #<STANDARD-METHOD GREET :AFTER (CHILD) {10075B71F3}>

(defmethod greet :around ((obj child))
  (format t "Hello my dear~&"))
;; #<STANDARD-METHOD GREET :AROUND (CHILD) {10076658E3}>

(defmethod greet :around ((obj child))
  (format t "Hello my dear~&")
  (when (next-method-p)
    (call-next-method)))
;; #<standard-method greet :around (child) {100AF76863}>

(fmakunbound 'greet)  ;; with Slime: C-c C-u (slime-undefine-function)

(defmethod greet ((obj person) &key talkative)
  (format t "Hello ~a~&" (name obj))
  (when talkative
    (format t "blah")))

(defgeneric greet (obj &key &allow-other-keys)
  (:documentation "say hi"))

(defmethod greet (obj &key &allow-other-keys)
  (format t "Are you a person ? You are a ~a.~&" (type-of obj)))

(defmethod greet ((obj person) &key talkative &allow-other-keys)
  (format t "Hello ~a !~&" (name obj))
  (when talkative
    (format t "blah")))

;(defun foo (&rest args &key &allow-other-keys)
 ;   (print args))

(fmakunbound 'greet)

(defgeneric greet (obj)
  (:documentation "say hello")
  (:method (obj)
    (format t "Are you a person ? You are a ~a~&." (type-of obj)))
  (:method ((obj person))
    (format t "Hello blya ~a !~&" (name obj)))
  (:method ((obj child))
    (format t "SDFur so cute~&")))

(defclass meal-type ()
    ((processor
        :initform 10
        :accessor processor
        :initarg  :processor)))

(defvar m1 (make-instance 'meal-type))

(defgeneric feed (obj meal-type)
  (:method (obj meal-type)
    (declare (ignorable meal-type))
    (format t "eating~&")))

(defmethod feed (obj (meal-type (eql :dessert)))
    (declare (ignorable meal-type))
    (format t "mmh, dessert !~&"))

(defmethod feed ((obj child) (meal-type (eql :soup)))
    (declare (ignorable meal-type))
    (format t "bwark~&"))

(defmethod feed ((obj child) (huel (eql :huyup)))
    (declare (ignorable meal-type))
    (format t "bribribri~&"))

(defparameter *a-string* "A")

(defmethod feed ((obj string) (meal-type (eql *a-string*)))
    (format t "string-string-string"))

(defmethod some-fn ((num (eql *a-string*)))
  (print "a specific string"))

(defun funcIngnore (x y)
    (declare (ignorable y)) (+ x y))

(defgeneric hug (a b)
    (:documentation "Hug between two persons.")
    (:method (a b)
    (format t "asdfsdf")))
;; #<STANDARD-GENERIC-FUNCTION HUG (0)>

(defmethod hug ((a person) (b person))
  :person-person-hug)

(defmethod hug ((a person) (b child))
  :person-child-hug)

(defvar p2 (make-instance 'person :name "Mordecai" ))

(defmethod (setf name) (new-val (obj child))
  (if (equalp new-val "james bond")
    (format t "Dude that's not possible.~&")
    (setf (slot-value obj 'name) new-val)))

(defparameter *some-array* (make-array 10 :initial-contents '(12 32 32 4 8 7 2 1 111 999)))

(defun arr-index (index-string)
  (aref *some-array* (parse-integer index-string)))

(defun (setf arr-index) (new-value index-string)
  (setf (aref *some-array* (parse-integer index-string)) new-value))

(defgeneric dishes (obj)
   (:method-combination progn)
   (:method progn (obj)
     (format t "- clean and dry.~&"))
   (:method progn ((obj person))
     (format t "- bring a person's dishes~&"))
   (:method progn ((obj child))
     (format t "- bring the baby dishes~&")))
;; #<STANDARD-GENERIC-FUNCTION DISHES (3)>

(defgeneric tidy (obj)
  (:method-combination list)
    (:method list (obj)
      :foo)
  (:method list ((obj person))
    :books)
  (:method list ((obj child))
    :toys))
;; #<STANDARD-GENERIC-FUNCTION TIDY (3)>

(defgeneric foo (x)
  (:method (x) 3))
(defmethod foo :around ((x fixnum))
  (format t "Around")
  (1+ (call-next-method)))
(defmethod foo ((x integer))
  (format t "Integer")
  (* 2 (call-next-method)))
(defmethod foo ((x float))
  (format t "Float")
  (* 3 (call-next-method)))
(defmethod foo :before ((x single-float))
  (format t "Single-float")
  )
(defmethod foo :after ((x double-float))
 (format t "Double-float")
 )

(defgeneric foo1 (x)
  (:method (x) 3))
(defmethod foo1 ((x integer))
  (format t "Integer")
  (* 2 (call-next-method)))
(defmethod foo1 ((x float))
  (format t "Float")
  (* 3 (call-next-method)))
(defmethod foo1 :before ((x integer))
  (format t "Single-float")
  x)
(defmethod foo1 :after ((x float))
 (format t "Double-float")
    x)

(defclass counted-class (standard-class)
  ((counter :initform 0)))
;#<STANDARD-CLASS COUNTED-CLASS>

(defmethod sb-mop:validate-superclass ((class counted-class)
                                          (super standard-class)) t)

;;(unintern 'person)
;; this is necessary to change the metaclass of person.
(setf (find-class 'person) nil)
;; https://stackoverflow.com/questions/38811931/how-to-change-classs-metaclass#38812140

(defclass person ()
  ((name
    :initarg :name
    :accessor name))
  (:metaclass counted-class)) ;; <- metaclass
;; #<COUNTED-CLASS PERSON>
;;   ^^^ not standard-class anymore.

(defmethod make-instance :after ((class counted-class) &key)
  (incf (slot-value class 'counter)))
;; #<STANDARD-METHOD MAKE-INSTANCE :AFTER (COUNTED-CLASS) {1007718473}>

(defmethod INITIALIZE-INSTANCE :after ((obj person) &key)
  (with-slots (name) obj
    (assert (>= (length name) 3)
            (name)  ;; creates a restart that offers to change "name"
            "The value of name is ~a. It should be longer than 3 characters." name)))


