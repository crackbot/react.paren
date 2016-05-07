
(defpackage :react.paren
  (:use :cl
        :parenscript :contracts.paren :serve.paren
        :mgl-pax :mgl-pax-ext)
  (:export :component
           :defcomponent
           :render-component
           :render
           :who
           :set-state%
           :create-el
           :@main-manual))

(defpackage :react.paren-tests
  (:use :cl :parenscript :react.paren :lisp-unit))
