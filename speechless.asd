#|
 This file is a part of speechless
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem speechless
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A dialogue system language implementation."
  :homepage "https://shirakumo.github.io/speechless"
  :bug-tracker "https://github.com/shirakumo/speechless/issues"
  :source-control (:git "https://github.com/shirakumo/speechless.git")
  :serial T
  :components ((:file "package")
               (:file "components")
               (:file "syntax")
               (:file "instructions")
               (:file "compiler")
               (:file "optimizers")
               (:file "vm")
               (:file "documentation"))
  :depends-on (:cl-markless
               :documentation-utils))
