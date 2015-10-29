
(defsystem :react.paren
  :name "react.paren"
  :description "Bindings to react.js library with several extensions."
  :version "0.0.2"
  :author "Crackbot <thecrackbot@gmail.com>"
  :maintainer "Crackbot <thecrackbot@gmail.com>"
  :license "The MIT License (MIT)"
  :components ((:static-file "react.paren.asd")
               (:file "package")
               (:file "pax")
               (:file "react")
               (:module "lib"
                        :components ((:javascript-file "react-dom-0.14.0.js")
                                     (:javascript-file "react-with-addons-0.14.0.js"))))
  :depends-on (:parenscript :contracts.paren :iterate :mgl-pax :serve.paren :lisp-unit))
