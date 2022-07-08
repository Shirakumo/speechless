(in-package #:org.shirakumo.fraf.speechless)

(defvar *optimization-passes* '(jump-resolution-pass noop-elimination-pass))

(defclass pass () ())

(defmethod run-pass ((pass symbol) thing)
  (run-pass (make-instance pass) thing))

(defmethod run-pass ((pass pass) (assembly assembly))
  (let ((*root* assembly))
    (loop for instruction across (instructions assembly)
          do (run-pass pass instruction)))
  assembly)

(defmethod run-pass ((pass pass) (instruction instruction)))

(defun optimize-instructions (assembly)
  (loop for pass in *optimization-passes*
        do (run-pass pass assembly))
  assembly)

(defun compile* (thing &optional (assembly T))
  (multiple-value-bind (assembly root) (compile thing assembly)
    (let ((*root* root))
      (optimize-instructions assembly))))

(defclass jump-resolution-pass (pass)
  ((label-map :initform (make-hash-table :test 'eq) :reader label-map)))

(defun find-label (component)
  (loop for key being the hash-keys of (mcomponents:labels *root*)
        for value being the hash-values of (mcomponents:labels *root*)
        do (when (eql value component) (return key))))

(defmethod run-pass :before ((pass jump-resolution-pass) (assembly assembly))
  ;; Gather all labels into a table
  (loop for instruction across (instructions assembly)
        do (setf (gethash (label instruction) (label-map pass)) (index instruction))
           (setf (label instruction) (find-label (label instruction)))))

;; Resolve jump targets
(defmethod run-pass ((pass jump-resolution-pass) (instruction jump))
  (setf (target instruction) (or (when (integerp (target instruction)) (target instruction))
                                 (gethash (target instruction) (label-map pass))
                                 (error "Jump to unknown target: ~s" (target instruction)))))

(defclass noop-elimination-pass (pass)
  ((label-map :initform (make-hash-table :test 'eq) :reader label-map)))

(defun find-new-index (target)
  (if (< target (length (instructions *root*)))
      (loop with instructions = (instructions *root*)
            for i from target below (length instructions)
            for other = (aref instructions i)
            while (typep other 'noop)
            finally (return (index other)))
      target))

(defmethod run-pass :before ((pass noop-elimination-pass) (assembly assembly))
  ;; Map all instruction indices to ones as if noops did not exist.
  (loop with i = 0
        for instruction across (instructions assembly)
        do (unless (typep instruction 'noop)
             (setf (index instruction) i)
             (incf i))))

;; Rewrite jumps to such that they point to the index after any noops
(defmethod run-pass ((pass noop-elimination-pass) (instruction jump))
  (setf (target instruction) (find-new-index (target instruction))))

;; Rewrite noops themselves to pass the label on to following instruction
(defmethod run-pass ((pass noop-elimination-pass) (instruction noop))
  (when (label instruction)
    (let ((new (loop for i from (index instruction) below (length (instructions *root*))
                     do (unless (typep (aref (instructions *root*) i) 'noop)
                          (return i))
                     finally (return (index instruction)))))
      (setf (label (aref (instructions *root*) new)) (label instruction)))))

(defmethod run-pass ((pass noop-elimination-pass) (instruction conditional))
  (loop for clause in (clauses instruction)
        do (setf (cdr clause) (find-new-index (cdr clause)))))

(defmethod run-pass ((pass noop-elimination-pass) (instruction dispatch))
  (map-into (targets instruction) #'find-new-index (targets instruction)))

(defmethod run-pass :after ((pass noop-elimination-pass) (assembly assembly))
  ;; Remove any noops from the instructions
  (setf (instructions assembly)
        (delete-if (lambda (instruction)
                     (typep instruction 'noop))
                   (instructions assembly))))

;; TODO: dead code elimination
;; TODO: ensure all jump targets are in-bounds
;; TODO: consistency checkers
