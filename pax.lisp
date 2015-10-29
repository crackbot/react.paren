
(in-package :react.paren)

(define-locative-type psmacro ())

(defmethod locate-object (symbol (locative-type (eql 'psmacro)) locative-args)
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'psmacro))
                                locative-args stream)
  (declare (ignore locative-args))
  (format stream "- [psmacro] ")
  (mgl-pax::print-name (prin1-to-string symbol) stream)
  (write-char #\Space stream)
  (terpri stream))
