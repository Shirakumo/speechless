(asdf:load-system :staple-markless)

(defpackage "speechless-docs"
  (:use #:cl)
  (:local-nicknames
   (#:dialogue #:org.shirakumo.fraf.speechless)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "zippy-docs")))

(defmethod staple:page-type ((system (eql (asdf:find-system :speechless))))
  'page*)

(defmethod staple:packages ((system (eql (asdf:find-system :speechless))))
  (mapcar #'find-package '(:org.shirakumo.fraf.speechless
                           :org.shirakumo.fraf.speechless.syntax
                           :org.shirakumo.fraf.speechless.components)))
