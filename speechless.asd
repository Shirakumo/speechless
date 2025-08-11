(asdf:defsystem speechless
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A dialogue system language implementation."
  :homepage "https://shirakumo.org/docs/speechless"
  :bug-tracker "https://shirakumo.org/project/speechless/issues"
  :source-control (:git "https://shirakumo.org/project/speechless.git")
  :build-operation "program-op"
  :build-pathname
  #+windows "speechless.exe"
  #+linux "speechless.run"
  #-(or windows linux) "speechless.o"
  :entry-point "org.shirakumo.fraf.speechless::main"
  :serial T
  :components ((:file "package")
               (:file "components")
               (:file "syntax")
               (:file "instructions")
               (:file "compiler")
               (:file "optimizers")
               (:file "vm")
               (:file "diff")
               (:file "printer")
               (:file "documentation"))
  :depends-on (:cl-markless
               :documentation-utils))
