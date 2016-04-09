
(defpackage :react.paren
  (:use :cl :parenscript :contracts.paren :serve.paren :mgl-pax :mgl-pax-ext)
  (:export :component
           :defcomponent
           :render
           :render-component           
           :who
           :set-state%))

(defpackage :react.paren-tests
  (:use :cl :react.paren :parenscript :lisp-unit))
