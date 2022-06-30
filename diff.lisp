(in-package #:org.shirakumo.fraf.speechless)

(defvar *source*)
(defvar *location-table*)
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

(defun diff (*source* b &optional differ)
  (multiple-value-bind (a parser) (ensure-parsed *source*)
    (let ((*location-table* (org.shirakumo.fraf.speechless.syntax::location-table parser))
          (*component-path* ()))
      (%diff a
             (ensure-parsed b)
             (etypecase differ
               (differ differ)
               (null (make-instance 'localisation-differ))
               ((or symbol class) (make-instance differ)))))))

(defun compare (format b a)
  (unless (equal a b)
    (diff-mismatch format b a)))

(defclass localisation-differ (differ) ())

(defmethod %diff progn (a b (differ localisation-differ))
  (compare "Expected ~a, but got ~a" (unlist (type-of b)) (unlist (type-of a))))

(define-diff localisation-differ vector (a b)
  (loop for i from 0 below (min (length a) (length b))
        do (%diff (aref a i) (aref b i) differ)))

(define-diff localisation-differ cons (a b)
  (compare "Expected ~d elements, but got ~d." (length b) (length a))
  (loop for ae in a
        for be in b
        do (%diff ae be differ)))

(define-diff localisation-differ mcomponents:parent-component (a b)
  (let ((achildren (mcomponents:children a))
        (bchildren (mcomponents:children b)))
    (loop with ai = 0
          with bi = 0
          while (and (< ai (length achildren))
                     (< bi (length bchildren)))
          do (let ((aelement (aref achildren ai))
                   (belement (aref bchildren bi)))
               (cond ((typep aelement '(or string mcomponents:newline mcomponents:en-dash mcomponents:em-dash))
                      (incf ai))
                     ((typep belement '(or string mcomponents:newline mcomponents:en-dash mcomponents:em-dash))
                      (incf bi))
                     (T
                      (%diff aelement belement differ)
                      (incf ai)
                      (incf bi)))))))

(define-diff localisation-differ real (a b)
  (compare "Expected ~a, but got ~a." b a))

(define-diff localisation-differ symbol (a b)
  (compare "Expected ~a, but got ~a." b a))

(define-diff localisation-differ mcomponents:ordered-list-item (a b)
  (%diff (mcomponents:number a) (mcomponents:number b) differ))

(define-diff localisation-differ mcomponents:unordered-list (a b)
  (compare "Number of choices is different. Expected ~d, but got ~d"
           (length (mcomponents:children b)) (length (mcomponents:children a))))

(define-diff localisation-differ mcomponents:directives-instruction (a b)
  (%diff (mcomponents:directives a) (mcomponents:directives b) differ))

(define-diff localisation-differ mcomponents:set (a b)
  (%diff (mcomponents:variable a) (mcomponents:variable b) differ)
  (%diff (mcomponents:value a) (mcomponents:value b) differ))

(define-diff localisation-differ mcomponents:include (a b)
  (compare "Expected file ~a, but got ~a." (mcomponents:file b) (mcomponents:file a)))

(define-diff localisation-differ mcomponents:targeted (a b)
  (compare "Expected target ~a, but got ~a." (mcomponents:target b) (mcomponents:target a)))

(define-diff localisation-differ mcomponents:sized (a b)
  (compare "Expected unit ~a, but got ~a." (mcomponents:unit b) (mcomponents:unit a))
  (compare "Expected size ~a, but got ~a." (mcomponents:size b) (mcomponents:size a)))

(define-diff localisation-differ mcomponents:embed (a b)
  (%diff (mcomponents:options a) (mcomponents:options b) differ))

(define-diff localisation-differ mcomponents:compound (a b)
  (%diff (mcomponents:options a) (mcomponents:options b) differ))

(define-diff localisation-differ mcomponents:color-option (a b)
  (let ((a (list (mcomponents:red a) (mcomponents:green a) (mcomponents:blue a)))
        (b (list (mcomponents:red b) (mcomponents:green b) (mcomponents:blue b))))
    (compare "Expected size ~a, but got ~a." b a)))

(define-diff localisation-differ components:conditional (a b)
  (compare "Number of clauses is different. Expected ~d, but got ~d"
           (length (components:clauses b)) (length (components:clauses a)))
  (loop for (a-predicate . a-children) across (components:clauses a)
        for (b-predicate . b-children) across (components:clauses b)
        do (compare "Expected form~%  ~s~%but got~%  ~s" a-predicate b-predicate)
           (%diff a-children b-children differ)))

(define-diff localisation-differ components:source (a b)
  (compare "Expected source ~a, but got ~a." (components:name b) (components:name a)))

(define-diff localisation-differ components:placeholder (a b)
  (compare "Expected form~%  ~s~%but got~%  ~s" (components:form b) (components:form a)))

(define-diff localisation-differ components:emote (a b)
  (compare "Expected emote ~a, but got ~a." (components:emote b) (components:emote a)))

(define-diff localisation-differ components:conditional-part (a b)
  (compare "Expected form~%  ~s~%but got~%  ~s" (components:form b) (components:form a))
  (compare "Number of choices is different. Expected ~d, but got ~d" (length (components:choices b)) (length (components:choices a)))
  (loop for a-choices across (components:choices a)
        for b-choices across (components:choices b)
        do (%diff a-choices b-choices differ)))

(define-diff localisation-differ components:speed (a b)
  (compare "Expected speed ~a, but got ~a." (components:speed a) (components:speed b)))

(define-diff localisation-differ components:setf (a b)
  (compare "Expected place~%  ~s~%but got~%  ~s" (components:place b) (components:place a))
  (compare "Expected form~%  ~s~%but got~%  ~s" (components:form b) (components:form a)))

(define-diff localisation-differ components:eval (a b)
  (compare "Expected form~%  ~s~%but got~%  ~s" (components:form b) (components:form a)))

(defun main ()
  #+sbcl (sb-ext:disable-debugger)
  (let* ((args (uiop:command-line-arguments))
         (command (or (pop args) "help")))
    (handler-case
        (handler-bind ((warning #'muffle-warning))
          (cond ((string-equal command "help")
                 (format *error-output* "Speechless command line utility V~a

Usage:
~a diff new-file base-file
  Compares the contents of NEW-FILE against BASE-FILE and
  emits warnings for any semantic changes that would alter
  the flow or execution of the speechless dialogue.
~:*
~a parse file
  Emits the syntax tree of FILE in a human-readable way.
  Can be used to debug the syntax of a file.
~:*
~a compile file
  Emits the assembly of FILE in a human-readable way.
  Can be used to debug the resulting program flow of a file.
~:*
~a help
  Shows this help text
"
                         #.(asdf:component-version (asdf:find-system "speechless")) (uiop:argv0)))
                ((string-equal command "parse")
                 (cl-markless:output (parse (uiop:parse-native-namestring (pop args))) :format 'cl-markless:debug :target *standard-output*))
                ((string-equal command "compile")
                 (disassemble (compile (parse (uiop:parse-native-namestring (pop args))) T)))
                ((string-equal command "diff")
                 (handler-bind ((warning (lambda (w)
                                           (format *error-output* "~&~a~%" w))))
                   (diff (uiop:parse-native-namestring (pop args)) (uiop:parse-native-namestring (pop args)))))
                (T
                 (error "No such command ~a" command))))
      (error (e)
        (format *error-output* "Error: ~a" e)))))
