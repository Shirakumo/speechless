(in-package #:org.shirakumo.fraf.speechless.components)

(defclass manual-newline (components:newline)
  ())

(defclass jump (components:block-component components:targeted)
  ())

(defmethod print-object ((jump jump) stream)
  (print-unreadable-object (jump stream :type T)
    (format stream "~a" (components:target jump))))

(defclass conditional (components:block-component)
  ((clauses :initform (make-array 0 :adjustable T :fill-pointer T) :accessor clauses)))

(defmethod markless:output-component ((c conditional) (s stream) (f markless:debug))
  (format s "/~a" (type-of c))
  (let ((markless::*level* (1+ markless::*level*)))
    (loop for (predicate . children) across (clauses c)
          do (let ((markless::*level* (1- markless::*level*)))
               (markless:output-component (format NIL "-- ~s" predicate) s f))
             (loop for child across children
                   do (markless:output-component child s f)))))

(defmethod markless:count-words-by + (method (conditional conditional))
  (loop for (predicate . children) across (clauses conditional)
        sum (loop for child across children
                  sum (markless:count-words-by method child))))

(defclass source (components:blockquote-header)
  ((name :initarg :name :initform (error "NAME required") :accessor name)))

(defmethod print-object ((source source) stream)
  (print-unreadable-object (source stream :type T)
    (format stream "~a" (name source))))

(defclass placeholder (components:inline-component)
  ((form :initarg :form :initform (error "FORM required") :accessor form)))

(defmethod print-object ((placeholder placeholder) stream)
  (print-unreadable-object (placeholder stream :type T)
    (format stream "~a" (form placeholder))))

(defclass emote (components:inline-component)
  ((emote :initarg :emote :initform (error "EMOTE required") :accessor emote)))

(defmethod print-object ((emote emote) stream)
  (print-unreadable-object (emote stream :type T)
    (format stream "~a" (emote emote))))

(defclass conditional-part (components:inline-component)
  ((form :initarg :form :initform (error "FORM required") :accessor form)
   (choices :initform (make-array 0 :adjustable T :fill-pointer T) :accessor choices)))

(defmethod components:children ((_ conditional-part))
  (aref (choices _) (1- (length (choices _)))))

(defmethod markless:output-component ((c conditional-part) (s stream) (f markless:debug))
  (format s "/~a" (type-of c))
  (let ((markless::*level* (1+ markless::*level*)))
    (loop for children across (choices c)
          do (loop for child across children
                   do (markless:output-component child s f))
             (let ((markless::*level* (1- markless::*level*)))
               (markless:output-component "--" s f)))))

(defmethod markless:count-words-by + (method (conditional conditional-part))
  (loop for choice across (choices conditional)
        sum (loop for child across choice
                  sum (markless:count-words-by method child))))

(defclass fake-instruction (components:instruction) ())

(defclass go (fake-instruction components:targeted)
  ())

(defmethod print-object ((go go) stream)
  (print-unreadable-object (go stream :type T)
    (format stream "~a" (components:target go))))

(defclass speed (fake-instruction)
  ((speed :initarg :speed :initform (error "SPEED required") :accessor speed)))

(defmethod print-object ((speed speed) stream)
  (print-unreadable-object (speed stream :type T)
    (format stream "~a" (speed speed))))

(defclass camera (fake-instruction)
  ((action :initarg :action :initform (error "ACTION required") :accessor action)
   (arguments :initarg :arguments :initform () :accessor arguments)))

(defmethod print-object ((camera camera) stream)
  (print-unreadable-object (camera stream :type T)
    (format stream "~s~{ ~s~}" (action camera) (arguments camera))))

(defclass move (fake-instruction)
  ((entity :initarg :entity :initform (error "ENTITY required") :accessor entity)
   (target :initarg :target :initform (error "TARGET required") :accessor components:target)))

(defmethod print-object ((move move) stream)
  (print-unreadable-object (move stream :type T)
    (format stream "~s -> ~s" (entity move) (components:target move))))

(defclass setf (fake-instruction)
  ((places :initarg :places :initform (error "PLACES required") :accessor places)))

(defmethod print-object ((setf setf) stream)
  (print-unreadable-object (setf stream :type T)
    (format stream "~{~s~^ ~}" (places setf))))

(defclass eval (fake-instruction)
  ((form :initarg :form :initform (error "FORM required") :accessor form)))

(defmethod print-object ((eval eval) stream)
  (print-unreadable-object (eval stream :type T)
    (format stream "~s" (form eval))))

(defmethod markless:output-component ((component eval) (stream stream) (_ markless:debug))
  (format stream " ~a ~s" (type-of component) (form component)))
