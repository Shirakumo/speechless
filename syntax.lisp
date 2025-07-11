(in-package #:org.shirakumo.fraf.speechless.syntax)

(defvar *default-directives*
  (list* 'placeholder 'emote
         'conditional-part 'part-separator
         'jump 'label 'conditional 'source
         'manual-newline
         (remove-if (lambda (s) (find s '(markless:code markless:blockquote-header markless:ordered-list markless:newline)))
                    markless:*default-directives*)))

(defvar *default-instruction-types*
  (list* 'components:go 'components:speed 'components:camera 'components:move 'components:setf 'components:eval
         markless:*default-instruction-types*))

(defclass parser (markless:parser)
  ((location-table :initform (make-hash-table :test 'eq) :reader location-table))
  (:default-initargs :directives *default-directives*
                     :instruction-types *default-instruction-types*))

(defmacro mark-component ()
  `(let* ((stack (markless:stack parser))
          (component (markless:stack-entry-component (markless:stack-top stack))))
     (typecase component
       (mcomponents:parent-component
        (let* ((children (mcomponents:children component))
               (prevlen (length children)))
          (prog1 (call-next-method)
            (loop for i from prevlen below (length children)
                  unless (gethash (aref children i) (location-table parser))
                  do (setf (gethash (aref children i) (location-table parser))
                           (list markless::*current-line-number* cursor))))))
       (T
        (call-next-method)))))

(defmethod markless:begin :around (directive (parser parser) line cursor)
  (mark-component))

(defmethod markless:invoke :around (directive component (parser parser) line cursor)
  (mark-component))

(defclass jump (markless:singular-line-directive)
  ())

(defmethod markless:prefix ((_ jump))
  #("<" " "))

(defmethod markless:begin ((_ jump) parser line cursor)
  (let ((component (make-instance 'components:jump :target (subseq line (+ 2 cursor)))))
    (markless:commit _ component parser))
  (length line))

(defclass label (markless:singular-line-directive)
  ())

(defmethod markless:prefix ((_ label))
  #(">" " "))

(defmethod markless:begin ((_ label) parser line cursor)
  (let ((component (make-instance 'mcomponents:label :target (subseq line (+ 2 cursor)))))
    (markless:commit _ component parser))
  (+ 2 cursor))

(defmethod markless:invoke ((_ label) component parser line cursor)
  (markless:evaluate-instruction component parser)
  (length line))

(defmethod mcomponents:children ((_ components:conditional))
  (cdr (aref (components:clauses _) (1- (length (components:clauses _))))))

(defclass conditional (markless:block-directive)
  ())

(defmethod markless:prefix ((_ conditional))
  #("?" " "))

(defmethod markless:begin ((_ conditional) parser line cursor)
  (multiple-value-bind (form cursor) (read-from-string line T NIL :start (+ 2 cursor))
    (when (< cursor (length line))
      (error 'markless:parser-error :cursor cursor))
    (let ((component (make-instance 'components:conditional)))
      (vector-push-extend (cons form (make-array 0 :adjustable T :fill-pointer T))
                          (components:clauses component))
      (markless:commit _ component parser)))
  (length line))

(defmethod markless:consume-prefix ((_ conditional) component parser line cursor)
  (or (markless:match! "| " line cursor)
      (when (markless:match! "|?" line cursor)
        (if (< (+ 2 cursor) (length line))
            (multiple-value-bind (form cursor) (read-from-string line T NIL :start (+ 2 cursor))
              (when (< cursor (length line))
                (error 'markless:parser-error :cursor cursor))
              (vector-push-extend (cons form (make-array 0 :adjustable T :fill-pointer T))
                                  (components:clauses component)))
            (vector-push-extend (cons T (make-array 0 :adjustable T :fill-pointer T))
                                (components:clauses component)))
        (1+ (length line)))))

(defclass source (markless:blockquote-header)
  ())

(defmethod markless:begin ((_ source) parser line cursor)
  (multiple-value-bind (name cursor) (read-from-string line T NIL :start (+ 2 cursor))
    (when (< cursor (length line))
      (error 'markless:parser-error :cursor cursor))
    (check-type name symbol)
    (let* ((top (markless:stack-top (markless:stack parser)))
           (children (mcomponents:children (markless:stack-entry-component top)))
           (predecessor (when (< 0 (length children))
                          (aref children (1- (length children)))))
           (component (make-instance 'components:source :name name)))
      (when (and (typep predecessor 'mcomponents:blockquote)
                 (null (mcomponents:source predecessor)))
        (setf (mcomponents:source predecessor) component))
      (vector-push-extend component children)
      cursor)))

(defvar *placeholder-readtable* (copy-readtable))

(set-macro-character #\} (lambda (stream char)
                           (declare (ignore char))
                           (error 'reader-error :stream stream))
                     NIL *placeholder-readtable*)

(defclass placeholder (markless:inline-directive)
  ())

(defmethod markless:prefix ((_ placeholder))
  #("{"))

(defmethod markless:begin ((_ placeholder) parser line cursor)
  (let ((*readtable* *placeholder-readtable*))
    (multiple-value-bind (form cursor) (read-from-string line T NIL :start (1+ cursor))
      (let* ((entry (markless:stack-top (markless:stack parser)))
             (children (mcomponents:children (markless:stack-entry-component entry))))
        (vector-push-extend (make-instance 'components:placeholder :form form) children)
        (unless (char= #\} (aref line cursor))
          (error 'markless:parser-error :cursor cursor))
        (+ 1 cursor)))))

(defclass emote (markless:inline-directive)
  ())

(defmethod markless:prefix ((_ emote))
  #("(" ":"))

(defmethod markless:begin ((_ emote) parser line cursor)
  (let* ((entry (markless:stack-top (markless:stack parser)))
         (children (mcomponents:children (markless:stack-entry-component entry)))
         (end (loop for i from (+ 2 cursor) below (length line)
                    do (when (char= #\) (char line i))
                         (return i)))))
    (unless end
      (error 'markless:parser-error :cursor cursor))
    (multiple-value-bind (emote cursor) (read-from-string line T NIL :start (+ 2 cursor) :end end)
      (when (< cursor end)
        (error 'markless:parser-error :cursor cursor))
      (vector-push-extend (make-instance 'components:emote :emote emote) children)
      (+ 1 end))))

(defclass part-separator (markless:inline-directive)
  ())

(defmethod markless:prefix ((_ part-separator))
  #("|"))

(defmethod markless:begin ((_ part-separator) parser line cursor)
  (let* ((component (markless:stack-entry-component (markless:stack-top (markless:stack parser)))))
    (incf cursor)
    (typecase component
      (components:conditional-part
       (vector-push-extend (make-array 0 :adjustable T :fill-pointer T) (components:choices component))
       cursor)
      (T
       (vector-push-extend "|" (mcomponents:children component))
       cursor))))

(defclass conditional-part (markless:inline-directive)
  ())

(defmethod markless:prefix ((_ conditional-part))
  #("["))

(defmethod markless:begin ((_ conditional-part) parser line cursor)
  (multiple-value-bind (form cursor) (read-from-string line T NIL :start (1+ cursor))
    (let ((component (make-instance 'components:conditional-part :form form)))
      (vector-push-extend (make-array 0 :adjustable T :fill-pointer T) (components:choices component))
      (markless:commit _ component parser))
    cursor))

(defmethod markless:invoke ((_ conditional-part) component parser line cursor)
  (markless:read-inline parser line cursor #\]))

(defmethod markless:consume-end ((_ conditional-part) component parser line cursor)
  (markless:match! "]" line cursor))

(defmethod markless:end :after ((_ conditional-part) component parser)
  ;; FIXME
  (markless::vector-push-front "[" (mcomponents:children component)))

(defmethod markless:evaluate-instruction ((instruction components::fake-instruction) parser))

(defmethod markless:parse-instruction ((proto components:go) line cursor)
  (make-instance (class-of proto) :target (subseq line cursor)))

(defmethod markless:parse-instruction ((proto components:speed) line cursor)
  (make-instance (class-of proto) :speed (cl-markless::parse-float line :start cursor)))

(defmethod markless:parse-instruction ((proto components:camera) line cursor)
  (destructuring-bind (action &rest args) (cl-markless::split-string line #\Space cursor)
    (make-instance (class-of proto) :action action :arguments args)))

(defmethod markless:parse-instruction ((proto components:move) line cursor)
  (destructuring-bind (entity target) (cl-markless::split-string line #\Space cursor)
    (make-instance (class-of proto) :entity (read-from-string entity)
                                    :target (read-from-string target))))

(defmethod markless:parse-instruction ((proto components:setf) line cursor)
  (let ((places ()))
    (loop
      (multiple-value-bind (place next) (read-from-string line T NIL :start cursor)
        (setf cursor next)
        (push place places)
        (when (<= (length line) cursor)
          (return (make-instance (class-of proto) :places (nreverse places))))))))

(defmethod markless:parse-instruction ((proto components:eval) line cursor)
  (let ((forms ()))
    (loop while (< cursor (length line))
          do (multiple-value-bind (form next) (read-from-string line T NIL :start cursor)
               (setf cursor next)
               (push form forms)))
    (make-instance (class-of proto) :form `(progn ,@(reverse forms)))))

(defclass manual-newline (markless:newline)
  ())

(defmethod markless:begin ((_ manual-newline) parser line cursor)
  (let* ((stack (markless:stack parser))
         (children (mcomponents:children (markless:stack-entry-component (markless:stack-top stack)))))
    (vector-push-extend (make-instance 'components:manual-newline) children)
    (+ 3 cursor)))
