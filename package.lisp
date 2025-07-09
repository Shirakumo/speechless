(defpackage #:org.shirakumo.fraf.speechless.components
  (:use #:cl)
  (:shadow #:go #:speed #:setf #:eval #:map)
  (:local-nicknames
   (#:markless #:org.shirakumo.markless)
   (#:components #:org.shirakumo.markless.components))
  (:export
   #:manual-newline
   #:jump
   #:placeholder
   #:form
   #:emote
   #:conditional-part
   #:choices
   #:conditional
   #:clauses
   #:source
   #:name
   #:go
   #:speed
   #:camera-instruction
   #:duration
   #:shake
   #:camera
   #:arguments
   #:move
   #:setf
   #:places
   #:eval))

(defpackage #:org.shirakumo.fraf.speechless.syntax
  (:use #:cl)
  (:local-nicknames
   (#:components #:org.shirakumo.fraf.speechless.components)
   (#:mcomponents #:org.shirakumo.markless.components)
   (#:markless #:org.shirakumo.markless))
  (:export
   #:*default-directives*
   #:*default-instruction-types*
   #:parser
   #:jump
   #:label
   #:conditional
   #:source
   #:placeholder
   #:emote
   #:part-separator
   #:conditional-part
   #:manual-newline))

(defpackage #:org.shirakumo.fraf.speechless
  (:use #:cl)
  (:shadow #:compile #:eval #:disassemble)
  (:local-nicknames
   (#:components #:org.shirakumo.fraf.speechless.components)
   (#:mcomponents #:org.shirakumo.markless.components)
   (#:markless #:org.shirakumo.markless))
  ;; diff.lisp
  (:export
   #:diff-mismatch
   #:location
   #:message
   #:differ
   #:define-diff
   #:diff
   #:localisation-differ)
  ;; instructions.lisp
  (:export
   #:instruction
   #:index
   #:label
   #:noop
   #:source
   #:name
   #:jump
   #:target
   #:conditional
   #:clauses
   #:emote
   #:pause
   #:placeholder
   #:choose
   #:commit-choice
   #:confirm
   #:clear
   #:begin-mark
   #:end-mark
   #:text
   #:eval)
  ;; compiler.lisp
  (:export
   #:parse
   #:compile
   #:disassemble
   #:wrap-lexenv
   #:assembly
   #:instructions
   #:next-index
   #:emit
   #:walk
   #:define-simple-walker
   #:define-markup-walker
   #:resolved-target)
  ;; optimizers.lisp
  (:export
   #:pass
   #:run-pass
   #:compile*
   #:optimize-instructions
   #:jump-resolution-pass
   #:noop-elimination-pass)
  ;; printer.lisp
  (:export
   #:speechless
   #:highlighted
   #:highlight)
  ;; vm.lisp
  (:export
   #:request
   #:input-request
   #:target-request
   #:target
   #:text-request
   #:text
   #:markup
   #:choice-request
   #:choices
   #:targets
   #:confirm-request
   #:clear-request
   #:emote-request
   #:emote
   #:pause-request
   #:duration
   #:source-request
   #:end-request
   #:vm
   #:instructions
   #:text-buffer
   #:choices
   #:markup
   #:execute
   #:text
   #:pop-text
   #:run
   #:reset
   #:resume
   #:suspend))
