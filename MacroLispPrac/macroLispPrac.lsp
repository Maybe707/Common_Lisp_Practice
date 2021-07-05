(defparameter v1 1)
(defparameter v2 2)
(defparameter v3 3)
(defparameter v4 4)
(defparameter v5 5)

(defun initializer ()
             (compile-file "macroLispPrac.lsp")
             (load "macroLispPrac.fasl")
             (setq v1 1)
             (setq v2 2)
             (setq v3 3)
             (setq v4 4)
             (setq v5 5)
             )

(defmacro setq2 (v1 v2 e)
    (list 'progn (list 'setq v1 e) (list 'setq v2 e)))

(defun setq2-function (v1 v2 e)
    (list 'progn (list 'setq v1 e) (list 'setq v2 e)))

(defmacro setq-reversible (e1 e2 direction)
    (case direction
        (:normal (list 'setq e1 e2))
        (:backward (list 'setq e2 e1))
        (t `(error "Unknown direction: ~a" ,direction))))

(defmacro setq3 (v1 v2 e)
    `(progn (setq ,v1 ,e) (setq ,@v2 ,@e)))

(defmacro explain-exp (exp)
    `(format t "~a = ~a" ',exp ',exp))

(defmacro echo (&rest forms)
  `(progn
     ,@(loop for form in forms collect `(format t "Form: ~a~%" ,form))
     ,@forms))

(defmacro explain-exp2 (exp)
    (format t "~a = ~a" exp exp))

(defun explain-exp3 (exp)
    (format t "~a = ~a" exp exp))

(defmacro myMacro (p1 p2)
    (list 'setq p1 p2))

(defmacro Macro2 (p1 p2)
    `(setq ,p1 ,p2))

(defmacro Macro3 (p1 p2 p3)
    `(list 'p1 ''p2 ,p3))

(defmacro Macro4 (p1 p2 p3)
    `(setq ,p1 `(setq ,',p2 ,',p3)))

(defmacro Macro5 (x z y)
    `(list ,x `(,',z ,,@y)))

(defmacro Macro6 (p1 p2)
    `(setq ,p1 `(,',p2)))

(defmacro Macro7 (p1 p2 p3)
    `(list ,p1 'p2 p3))

(defmacro Macro8 (p1 p2 p3)
    `(list ,p1 ,p2 ,p3))

(defmacro Macro9 (p1 p2)
    `(setq ,p1 `(,'p2)))

(defmacro Macro10 (p10 p15 p20)
    `(format t "~a = ~a ~a" ,p10 ',p15 'p20))

(defmacro Macro11 (p30 p40 p50)
    `(format t "~a ~a ~a ~a" ',p30 `(,'p40 ,,p50 ,,p50 ,''p40 ,',p30) ,p30 'p30))

(defmacro Macro12 (p1 p2)
    (list 'setq p1 p2))

(defmacro Macro13 (p1 p2 p3 p4)
    `(format t "~a ~a" 'p1 `(,,p2 ,',p1 ,'p1 `(,,,p3 `(,,,',p4 ''',,,,p4 ,,,'p4 ,',',',p4)))))

(defmacro Macro14 (p1 p2 p3)
    `(set p1 `(setq ,',p2 ,',p3)))

(defmacro Macro15 (p1 p2)
    `(set ',p1 ,p2))
             
(defmacro Macro16 (p1 p2 p3)
    `(set 'p1 `(setq ,',p2 ,',p3)))

(defmacro Macro17 (p1)
    `(format t "~a" p1))

(defmacro Macro18 (p1)
    `(format t "~a" ,p1))

(defmacro Macro19 (p1)
    `(format t "~a" 'p1))

(defmacro Macro20 (p1)
    `(format t "~a" ',p1))

(defmacro Macro21 (p1 p2 p3)
    `(setq ,p1 `(setq ,',p2 ,,p3)))
 
(defmacro Macro22 (p1 p2)
    `(set ',p1 ,p2))
             
(defmacro Macro23 (p1 p2 p3)
    `(set 'p1 `(setq ,',p2 ,',p3)))

(defmacro Macro24 (p1)
    (list 'format 't "~a" p1))

(defmacro Macro25 (p1 p2)
    (list 'setq 'p1 p2))

(defmacro Macro26 (p1 p2)
    (list 'format t "~a ~a ~a" 'p1 p2))

(defmacro Macro27 (p1 p2 p3 p4)
    (eval (eval `(set ',p1 `(setq ,',p2 `(setq ,',',p3 ,,,p4))))))

(defmacro Macro28 (p1 p2 p3 p4)
    `(format t "~a ~a" ',p1 `(setq ,',p2 `(set ,,',p3 ,,,p4))))

(defmacro Macro29 (p1 p2 p3)
    (eval `(set ',p1 `(setq ,',p2 ,,p3))))

(defmacro Macro30 (p1 p2 p3 p4 p5)
    `(set ',p1 `(setq ,',p2 `(setq ,',',p3 `(setq ,',',',p4 ,,,,p5)))))

(defmacro Macro31 (p1 p2 p3)
    (eval `(set ',p1 `(setq ,',p2 ,,p3))))

(defmacro Macro32 (p1 p2 p3 p4 p5)
    `(list ,p1 `(,,@p2 `(,',',p3 `(,',',',p4 `(,',',',',p5))))))

(defmacro Macro33 (p1 p2)
    `(list ,p1 `(,,@p2)))

(defmacro Macro34 (p1 p2 p3 p4 p5)
    `(list ',p1 `(,',p2 `(,',',p3 `(,,,''',p4 ,,,,p5)))))

(defmacro Macro35 (p1)
    `(format t "~a ~a" ',p1 ','p1))

(defun lambdaHren (x)
    (cond ((symbolp x) `(list (,x)))
        ((> x 10) (list x x))
        ((>= x 0) (list 'low))
        (t '())))
   
(defun mapCcan (ls)
    (mapcan #'lambdaHren ls))

(defmacro setq2 (v1 v2 e)
  (let ((tempvar (gensym)))
    `(let ((,tempvar ,e))
       (progn (setq ,v1 ,tempvar)
              (setq ,v2 ,tempvar)))))

(defun sqone (x)
  (let ((y (+ x 1))) (* y y)))

(defmacro sqone2 (x)
  `(let ((y (+ ,x 1))) (* y y)))

(defmacro /*/ (x)
  `(let ((y (+ ,x 1))) (* y y)))

(defmacro for2 (listspec exp)
  ;;           ^^ listspec = (x :in list), a list of length 3.
  ;;                    ^^ exp = the rest of the code.
  (cond
    ((and (= (length listspec) 3)
          (symbolp (first listspec))
          (eq (second listspec) ':in))
     `(mapcar (lambda (,(first listspec))
                ,exp)
              ,(third listspec)))
    (t (error "Ill-formed for spec: ~A" listspec))))

(defmacro build-symbol (&rest list)
  (let ((p (find-if (lambda (x)
                      (and (consp x)
                           (eq (car x) ':package)))
                    list)))
    (when p
      (setq list (remove p list)))
    (let ((pkg (cond ((eq (second p) 'nil)
                      nil)
                     (t `(find-package ',(second p))))))
      (cond (p
             (cond (pkg
                    `(values (intern ,(symstuff list) ,pkg)))
                   (t
                    `(make-symbol ,(symstuff list)))))
            (t
             `(values (intern ,(symstuff list))))))))

(defun symstuff (list)
  `(concatenate 'string
                ,@(for2 (x :in list)
                       (cond ((stringp x)
                              `',x)
                             ((atom x)
                              `',(format nil "~a" x))
                             ((eq (car x) ':<)
                              `(format nil "~a" ,(second x)))
                             ((eq (car x) ':++)
                              `(format nil "~a" (incf ,(second x))))
                             (t
                              `(format nil "~a" ,x))))))

(defun TestFun (&rest ls)
    ls)

(defun Bformat (list)
    (for2 (x :in list) (cond ((stringp x)
                                                x)
                                            (t
                                                (format nil "~a" 555)))))

;; (intern (eval (symstuff (list 'a))) ':keyword) ;; For build-symbol macro.

(defmacro macroPodstanovka ()
    `,(symstuff (list "asdf" '1 'a)))

(defmacro macroPodstanovka2 ()
    (concatenate 'string (list '"asdf" 'a)))

(defmacro macroPodstanovka3 (pkg)
    `(values (intern ,(symstuff (list "asdf")) ,pkg)))

(defun symstuff2 (list)
  `(
                ,@(for (x :in list)
                       (cond ((stringp x)
                              `',x)
                             ((atom x)
                              `',(format nil "~a" x))
                             ((eq (car x) ':<)
                              `(format nil "~a" ,(second x)))
                             ((eq (car x) ':++)
                              `(format nil "~a" (incf ,(second x))))
                             (t
                              `(format nil "~a" ,x))))))

(defun make-a-zip (y z)
    (vector 2 'zip y z))

(defun test-whether-zip (x)
  (and (vectorp x) (eq (aref x 1) 'zip)))

(defmacro odd-define (name buildargs)
  `(progn (defun ,(build-symbol make-a- (:< name))
                                ,buildargs
            (vector ,(length buildargs) ',name ,@buildargs))
          (defun ,(build-symbol test-whether- (:< name)) (x)
            (and (vectorp x) (eq (aref x 1) ',name)))))

(defvar *current-form*)

(defmacro ex (&optional (form nil form-supplied))
  `(progn
     (pprint (setq *current-form*
                   (macroexpand-1
                    ,(cond (form-supplied
                            `',form)
                           (t '*current-form*)))))
     (values)))

(defmacro fi (s &optional (k 0))
  `(progn
     (pprint (setq *current-form*
                   (find-nth-occurrence ',s *current-form* ,k)))
     (values)))

(defun find-nth-occurrence (s currentform k)
    (let ((sub (car currentform)))
        (cond ((equal s sub)
                  (format nil "~a ~a ~a" k s sub))
            ((equal sub nil)
                (format nil "Ti v poryadke Sanya???"))
              (t (incf k) (find-nth-occurrence s (cdr currentform) k)))))

(defmacro returnCurrentForm ()
    `*current-form*)

(defmacro testMacro (x)
    `(format nil "~a ~a ~a" ',x ,x 'x))

(defun func17 (x)
    (let ((c x))
        (format nil "~a" c)))

(defmacro macrosnick ()
    `(list setq eq car cdr))

(defmacro somemacro (x y c z)
    `(list `(,list ,x ,,y) `(list ,,c ,,z)))

(defmacro somemacro2 (x y c z)
    `(list (list ,x ,y) (list ,c ,z)))

(defmacro somemacro3 (x y c z)
    `(list (list x y) (list c z)))

(defun function5 ()
    `(format nil "~a" v1))

(defmacro NewMacro (x y z c v)
    `(format nil "~a ~a ~a ~a ~a" x 'y ,z ',c ''v))

(defun OptionalFunc (x &optional (y nil c))
    (format nil "~a ~a ~a" x y c))

(defmacro Macro333 (p1 p2 p3 p4 p5)
    `(set ',p1 `(setq ,',p2 `(setq ,',',p3 `(setq ,',',',p4 ,,,,p5)))))



