
(asdf:defsystem :react.paren
  :name "react.paren"
  :description "Bindings to react.js library with several extensions."
  :version "0.0.4"
  :author "Crackbot <thecrackbot@gmail.com>"
  :maintainer "Crackbot <thecrackbot@gmail.com>"
  :license "The MIT License (MIT)"
  :components ((:static-file "react.paren.asd")
               (:file "package")
               (:file "react")
               (:module "bower_components"
                        :components ((:javascript-file "react/react-with-addons")
                                     (:javascript-file "react/react-dom"))))
  :depends-on (:parenscript :contracts.paren :serve.paren :mgl-pax :mgl-pax-ext :lisp-unit))
