(in-package #:org.shirakumo.fraf.speechless)

(defvar *root*)

(defun parse (thing)
  (let ((parser (make-instance 'org.shirakumo.fraf.speechless.syntax:parser)))
    (values (cl-markless:parse thing parser) parser)))

(defmethod compile ((thing mcomponents:component) assembly)
  (walk thing assembly)
  (values assembly thing))

(defmethod compile (thing assembly)
  (compile (parse thing) assembly))

(defmethod compile (thing (assembly symbol))
  (compile thing (make-instance assembly)))

(defmethod compile (thing (assembly (eql T)))
  (compile thing (make-instance 'assembly)))

(defmethod disassemble ((thing string))
  (disassemble (compile* thing)))

(defgeneric wrap-lexenv (assembly form)
  (:method (_ form)
    `(progn ,form)))

(defun compile-form (assembly form)
  (cl:compile NIL `(lambda () ,(wrap-lexenv assembly form))))

(defclass assembly ()
  ((instructions :initform (make-array 0 :adjustable T :fill-pointer T) :accessor instructions)))

(defmethod disassemble ((assembly assembly))
  (loop for i from 0
        for instruction across (instructions assembly)
        do (format T "~&~2d  " (index instruction))
           (disassemble instruction)))

(defmethod next-index ((assembly assembly))
  (length (instructions assembly)))

(defmethod emit ((instruction instruction) (assembly assembly))
  (setf (index instruction) (next-index assembly))
  (vector-push-extend instruction (instructions assembly)))

(defgeneric walk (ast assembly))

(defmethod walk (thing assembly)
  assembly)

(defmethod walk (thing (assembly (eql T)))
  (let ((assembly (make-instance 'assembly)))
    (walk thing assembly)
    assembly))

(defmacro define-simple-walker (component instruction &rest initargs)
  `(defmethod walk ((,component ,component) (assembly assembly))
     (emit (make-instance ',instruction
                          :label ,component
                          ,@initargs)
           assembly)))

(defmacro define-markup-walker (component &body markup)
  `(defmethod walk :around ((,component ,component) (assembly assembly))
     (let ((end (make-instance 'end-mark)))
       (emit (make-instance 'begin-mark :label ,component
                                        :markup (progn ,@markup)
                                        :end end)
             assembly)
       (call-next-method)
       (emit end assembly))))

(defmethod walk ((component mcomponents:parent-component) (assembly assembly))
  (loop for child across (mcomponents:children component)
        do (walk child assembly)))

(defun resolved-target (component)
  (or (mcomponents:label (mcomponents:target component) *root*)
      (error "Label ~s cannot be resolved to any target component."
             (mcomponents:target component))))

(defmethod walk ((component mcomponents:root-component) (assembly assembly))
  (let ((*root* component))
    (call-next-method)))

(defmethod walk ((component mcomponents:blockquote-header) (assembly assembly))
  (emit (make-instance 'source :label component
                               :name (components:name component))
        assembly))

(defmethod walk :before ((component mcomponents:blockquote) (assembly assembly))
  (emit (make-instance 'clear) assembly))

(defmethod walk :after ((component mcomponents:blockquote) (assembly assembly))
  (emit (make-instance 'confirm) assembly))

(defmethod walk ((component components:conditional) (assembly assembly))
  (let ((conditional (make-instance 'conditional :label component)))
    (emit conditional assembly)
    (let* ((end (make-instance 'noop))
           (clauses (loop for (predicate . children) across (components:clauses component)
                          for index = (next-index assembly)
                          do (loop for child across children
                                   do (walk child assembly))
                             (emit (make-instance 'jump :target end) assembly)
                          collect (cons (compile-form assembly predicate) index))))
      (setf (clauses conditional) (append clauses (list (cons (lambda () T) (next-index assembly)))))
      (emit end assembly))))

(defmethod walk ((component mcomponents:unordered-list) (assembly assembly))
  (let ((choose (make-instance 'choose))
        (end (make-instance 'noop))
        (items (mcomponents:children component)))
    (loop for i from 0 below (length items)
          for item = (aref items i)
          for children = (mcomponents:children item)
          do (when (/= 0 (length children))
               (emit (make-instance 'noop :label item) assembly)
               (walk (aref children 0) assembly)
               (emit (make-instance 'commit-choice :target children) assembly)
               (emit (make-instance 'jump :target (if (< i (1- (length items)))
                                                      (aref items (1+ i))
                                                      choose))
                     assembly)
               (emit (make-instance 'noop :label children) assembly)
               (loop for i from 1 below (length children)
                     do (walk (aref children i) assembly))
               (emit (make-instance 'jump :target end) assembly)))
    (emit choose assembly)
    (emit end assembly)))

(defmethod walk ((string string) (assembly assembly))
  (emit (make-instance 'text :text string) assembly))

(defun random-indicator-p (form)
  (and (symbolp form) (equal "?" (symbol-name form))))

(defmethod walk ((component components:conditional-part) (assembly assembly))
  ;; Strip shitty spaces
  (loop for choice across (components:choices component)
        for e = (1- (length choice))
        do (case (length choice)
             (0)
             (1
              (when (typep (aref choice 0) 'string)
                (setf (aref choice 0) (string-trim " " (aref choice 0)))))
             (T
              (when (typep (aref choice 0) 'string)
                (setf (aref choice 0) (string-left-trim " " (aref choice 0))))
              (when (typep (aref choice e) 'string)
                (setf (aref choice e) (string-right-trim " " (aref choice e)))))))
  (let* ((len (length (components:choices component)))
         (func (if (random-indicator-p (components:form component))
                   (lambda () (random len))
                   (compile-form assembly `(if ,(components:form component) 0 1))))
         (dispatch (make-instance 'dispatch :func func
                                            :label component)))
    (emit dispatch assembly)
    (let* ((end (make-instance 'noop))
           (targets (append (loop for choice across (components:choices component)
                                  for index = (next-index assembly)
                                  do (loop for i from 0 below (length choice)
                                           for child = (aref choice i)
                                           do (unless (and (= i 0)
                                                           (typep child 'mcomponents:newline))
                                                (walk child assembly)))
                                     (emit (make-instance 'jump :target end) assembly)
                                  collect index)
                            (list (next-index assembly)))))
      (setf (targets dispatch) targets)
      (emit end assembly))))

(defmethod walk ((component mcomponents:header) (assembly assembly))
  (emit (make-instance 'jump :target most-positive-fixnum) assembly)
  (emit (make-instance 'noop :label component) assembly))

(define-markup-walker mcomponents:bold
  '((:bold T)))

(define-markup-walker mcomponents:underline
  '((:underline T)))

(define-markup-walker mcomponents:italic
  '((:italic T)))

(define-markup-walker mcomponents:strikethrough
  '((:strikethrough T)))

(define-markup-walker mcomponents:supertext
  '((:supertext T)))

(define-markup-walker mcomponents:subtext
  '((:subtext T)))

(define-markup-walker mcomponents:compound
  (loop for option in (mcomponents:options mcomponents:compound)
        collect (etypecase option
                  (mcomponents:bold-option '(:bold T))
                  (mcomponents:italic-option '(:italic T))
                  (mcomponents:underline-option '(:underline T))
                  (mcomponents:strikethrough-option '(:strikethrough T))
                  (mcomponents:spoiler-option '(:spoiler T))
                  (mcomponents:font-option (list :font (mcomponents:font-family option)))
                  (mcomponents:color-option (list :color (list
                                                          (mcomponents:red option)
                                                          (mcomponents:green option)
                                                          (mcomponents:blue option))))
                  (mcomponents:size-option (list :size (mcomponents:size option))))))

(define-simple-walker components:jump jump
  :target (resolved-target components:jump))

(define-simple-walker mcomponents:label noop)

(define-simple-walker mcomponents:footnote noop)

(define-simple-walker components:go jump
  :target (resolved-target components:go))

(defmethod walk ((component mcomponents:newline) (assembly assembly))
  (emit (make-instance 'confirm :label component) assembly)
  (emit (make-instance 'clear :label component) assembly))

(define-simple-walker components:eval eval
  :func (compile-form assembly (components:form components:eval)))

(define-simple-walker components:setf eval
  :func (compile-form assembly `(setf ,(components:place components:setf)
                                      ,(components:form components:setf))))

(define-simple-walker components:emote emote
  :emote (components:emote components:emote))

(define-simple-walker mcomponents:en-dash pause
  :duration 0.5)

(define-simple-walker mcomponents:em-dash pause
  :duration 1.0)

(define-simple-walker components:placeholder placeholder
  :func (compile-form assembly (components:form components:placeholder)))

;; TODO: implement the following
;; speed
;; move
;; zoom
;; roll
;; show
