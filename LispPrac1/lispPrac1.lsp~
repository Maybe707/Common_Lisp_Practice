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

(defvar c1 (make-instance 'child))

(defvar q1 (make-instance 'quakeTop1 :name "QuakeTop"))

(setf (find-class 'person) nil)
