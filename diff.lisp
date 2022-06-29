(in-package #:org.shirakumo.fraf.speechless)

(defvar *location-table*)
(defvar *source*)
(defvar *component-path*)

(defun format-location-in-source (location source)
  (etypecase source
    (pathname
     (with-open-file (stream source)
       (format-location-in-source location stream)))
    (string
     (with-input-from-string (stream source)
       (format-location-in-source location stream)))
    (stream
     (destructuring-bind (line col) location
       (with-output-to-string (out)
         (when (< 0 line)
           (loop repeat (1- line) do (read-line source NIL))
           (format out "~3d  ~a~%" (1- line) (read-line source NIL)))
         (format out "~3d> ~a~%~v@{─~}┘~%" line (read-line source NIL) (+ 5 col) NIL)
         (let ((read (read-line source NIL)))
           (when read (format out "~3d  ~a" (1+ line) read))))))))

(defun unlist (a)
  (if (listp a) (car a) a))

(define-condition diff-mismatch (warning)
  ((location :initarg :location :reader location)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "The syntax tree diverges from the expected~:[.~;~:* at~%~%~a~]~%~@[~%~a~]" (location c) (message c)))))

(defun diff-mismatch (format &rest args)
  (let ((location (loop for object in *component-path*
                        thereis (gethash object *location-table*))))
    (warn 'diff-mismatch :location (when location (format-location-in-source location *source*))
                         :message (apply #'format NIL format args))))

(defun ensure-parsed (thing)
  (etypecase thing
    ((or pathname string stream) (parse thing))
    (mcomponents:component thing)))

(defclass differ () ())
(defgeneric %diff (a b differ)
  (:method-combination progn))

(defmethod %diff :around (a b (differ differ))
  (let ((*component-path* (list* a *component-path*)))
    (call-next-method)))

(defmacro define-diff (differ type (a b) &body body)
  `(defmethod %diff progn ((,a ,type) (,b ,type) (differ ,differ))
     ,@body))

(define-diff differ mcomponents:parent-component (a b)
  (%diff (mcomponents:children a) (mcomponents:children b) differ))

(define-diff differ mcomponents:text-component (a b)
  (%diff (mcomponents:text a) (mcomponents:text b) differ))

(define-diff differ vector (a b)
  (loop for i from 0 below (min (length a) (length b))
        do (%diff (aref a i) (aref b i) differ)))

(define-diff differ cons (a b)
  (loop for ae in a
        for be in b
        do (%diff ae be differ)))

(defun diff (*source* b &optional differ)
  (multiple-value-bind (a parser) (ensure-parsed *source*)
    (let ((*location-table* (org.shirakumo.fraf.speechless.syntax::location-table parser))
          (*component-path* ()))
      (%diff a
             (ensure-parsed b)
             (etypecase differ
               (differ differ)
               (null (make-instance 'exact-differ))
               ((or symbol class) (make-instance differ)))))))

(defclass exact-differ (differ) ())

(defmethod %diff progn (a b (differ exact-differ))
  (unless (eql (unlist (type-of a)) (unlist (type-of b)))
    (diff-mismatch "Expected object of type~%  ~s~%but got~%  ~s" (type-of b) (type-of a))))

(define-diff exact-differ string (a b)
  (unless (string= a b)
    (diff-mismatch "The string~%  ~s~%does not match~%  ~s~%at position ~a~%" a b
                   (loop for i from 0 below (min (length a) (length b))
                         do (when (char/= (char a i) (char b i)) (return i))))))

(define-diff exact-differ vector (a b)
  (unless (stringp a)
    (when (/= (length a) (length b))
      (diff-mismatch "Expected ~d elements, but got ~d." (length b) (length a)))))

(define-diff exact-differ cons (a b)
  (when (/= (length a) (length b))
    (diff-mismatch "Expected ~d elements, but got ~d." (length b) (length a))))

(define-diff exact-differ real (a b)
  (when (/= a b)
    (diff-mismatch "Expected ~a, but got ~a." b a)))

(define-diff exact-differ symbol (a b)
  (unless (eq a b)
    (diff-mismatch "Expected ~a, but got ~a." b a)))

(define-diff exact-differ mcomponents:blockquote (a b))

(define-diff exact-differ mcomponents:ordered-list-item (a b)
  (%diff (mcomponents:number a) (mcomponents:number b) differ))

(define-diff exact-differ mcomponents:message-instruction (a b)
  (%diff (mcomponents:message a) (mcomponents:message b) differ))

(define-diff exact-differ mcomponents:directives-instruction (a b)
  (%diff (mcomponents:directives a) (mcomponents:directives b) differ))

(define-diff exact-differ mcomponents:set (a b)
  (%diff (mcomponents:variable a) (mcomponents:variable b) differ)
  (%diff (mcomponents:value a) (mcomponents:value b) differ))

(define-diff exact-differ mcomponents:include (a b)
  (%diff (mcomponents:file a) (mcomponents:file b) differ))

(define-diff exact-differ mcomponents:targeted (a b)
  (%diff (mcomponents:target a) (mcomponents:target b) differ))

(define-diff exact-differ mcomponents:sized (a b)
  (%diff (mcomponents:unit a) (mcomponents:unit b) differ)
  (%diff (mcomponents:size a) (mcomponents:size b) differ))

(define-diff exact-differ mcomponents:embed (a b)
  (%diff (mcomponents:options a) (mcomponents:options b) differ))

(define-diff exact-differ mcomponents:float-option (a b)
  (%diff (mcomponents:direction a) (mcomponents:direction b) differ))

(define-diff exact-differ mcomponents:compound (a b)
  (%diff (mcomponents:options a) (mcomponents:options b) differ))

(define-diff exact-differ mcomponents:language-option (a b)
  (%diff (mcomponents:language a) (mcomponents:language b) differ))

(define-diff exact-differ mcomponents:start-option (a b)
  (%diff (mcomponents:start a) (mcomponents:start b) differ))

(define-diff exact-differ mcomponents:end-option (a b)
  (%diff (mcomponents:end a) (mcomponents:end b) differ)
  (%diff (mcomponents:offset-p a) (mcomponents:offset-p b) differ))

(define-diff exact-differ mcomponents:encoding-option (a b)
  (%diff (mcomponents:encoding a) (mcomponents:encoding b) differ))

(define-diff exact-differ mcomponents:font-option (a b)
  (%diff (mcomponents:font-family a) (mcomponents:font-family b) differ))

(define-diff exact-differ mcomponents:color-option (a b)
  (%diff (mcomponents:red a) (mcomponents:red b) differ)
  (%diff (mcomponents:green a) (mcomponents:green b) differ)
  (%diff (mcomponents:blue a) (mcomponents:blue b) differ))


(define-diff exact-differ components:jump (a b)
  (%diff (mcomponents:target a) (mcomponents:target b) differ))

(define-diff exact-differ components:conditional (a b)
  (%diff (components:clauses a) (components:clauses b) differ))

(define-diff exact-differ components:source (a b)
  (%diff (components:name a) (components:name b) differ))

(define-diff exact-differ components:placeholder (a b)
  (%diff (components:form a) (components:form b) differ))

(define-diff exact-differ components:emote (a b)
  (%diff (components:emote a) (components:emote b) differ))

(define-diff exact-differ components:conditional-part (a b)
  (%diff (components:form a) (components:form b) differ)
  (%diff (components:choices a) (components:choices b) differ))

(define-diff exact-differ components:go (a b)
  (%diff (mcomponents:target a) (mcomponents:target b) differ))

(define-diff exact-differ components:speed (a b)
  (%diff (components:speed a) (components:speed b) differ))

(define-diff exact-differ components:setf (a b)
  (%diff (components:place a) (components:place b) differ)
  (%diff (components:form a) (components:form b) differ))

(define-diff exact-differ components:eval (a b)
  (%diff (components:form a) (components:form b) differ))
