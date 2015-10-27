
(in-package :react.paren)

(defpslib "react"
    :system-source-dir #p"/home/cb/dev/2014/react.paren"
    :paren-source-dir #p"/home/cb/dev/2014/react.paren/lib"
    :include-js t)

(defsection @react-manual (:title "React manual")
  "Manual for using react.js via parenscript"
  "This library define couple of helper macros to make it easier to
use react.js library"
  (*with-self* variable))

(defparameter *react-name* '*react)
(defparameter *with-self* t
  "Define self variable inside each function binded to this: var self
  = this;")

(defvar *supported-react-tags*
  '(a abbr address area article aside audio b base bdi bdo big blockquote body br
    button canvas caption cite code col colgroup data datalist dd del details dfn
    div dl dt em embed fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6
    head header hr html i iframe img input ins kbd keygen label legend li link
    main map mark menu menuitem meta meter nav noscript object ol optgroup option
    output p param pre progress q rp rt ruby s samp script section select small
    source span strong style sub summary sup table tbody td textarea tfoot th
    thead time title tr track u ul var video wbr)
    "Tags that are converted to react classes")

(defun parse-defun (def)
  (let ((defun-form (car def))
        (name (cadr def))
        (lambda-list (caddr def))
        (body (cdddr def)))
    (values defun-form name lambda-list body)))

(defun transform-defun-form (defun-form)
  ;'lambda/contract)
  (case defun-form
    (defun 'lambda)
    (defun/contract 'lambda/contract)))

(defun process-props (component-name props-contracts)
  ""
  (let ((res '()))
    (do ((i 0 (+ i 2)))
        ((>= i (length props-contracts)))
      (progn
        (let ((expected (with-output-to-string (out)
                          (format out "~A" (nth (+ i 1) props-contracts)))))
          (push (nth i props-contracts) res)
          (push `(lambda (props prop-name component)
                   (when (not (,(nth (+ i 1) props-contracts)
                               (getprop props prop-name))) ;; prop-name component-name))
                     (blame (create :type "react-component"
                                    :function ,(symbol-name component-name)
                                    :variable prop-name
                                    :given (getprop props prop-name)
                                    :expected ,expected))
                     (return (new (*error (+ "Prop " prop-name " validation failed"))))))
              res))))
    (nreverse res)))
  ;; (list (car props)
  ;;       (cadr props)))
  ;; customProp: function(props, propName, componentName) {
  ;;     if (!/matchme/.test(props[propName])) {
  ;;       return new Error('Validation failed!');
  ;;     }
  ;;   }

(defpsmacro component (name &rest body)
  "Define react component"
  (flet ((process-form (def)

           (cond ((equalp (car def) :>>)
                  ;; this adds support to define parenscript contracts
                  ;; inside the component, which is transformed into
                  ;; propTypes
                  `(prop-types (create ,@(process-props name (cdr def))))) ; mapcan #'process-props (cdr def)))))
                 
                  ((= (length def) 2)
                   `(,(car def) ,@(cdr def)))
                 
                 (t (multiple-value-bind (defun-form name lambda-list body)
                        (parse-defun def)
                      (case defun-form
                        (defun `(,name (lambda ,lambda-list
                                         ,(when *with-self*
                                                `(var self this))
                                         (progn ,@body))))
                        (defun/contract `(,name (lambda/contract ,lambda-list
                                                                 ,@body)))))))))
    
    `(chain ,*react-name* (create-class (create 'display-name ,(symbol-name name)
                                                ,@(mapcan #'process-form body))))))

(defpsmacro defcomponent (name &rest body)
  "Bind component to a variable of the same name as component"
  `(var ,name (component ,name ,@body)))

(defpsmacro render-component (name dom)
  "Render component inside dom node"
  `(chain ,*react-name* (render-component ,name ,dom)))

(defpsmacro who (&rest body)
  "Transform cl-who like forms into react nodes"
  (when (and (listp body) (listp (car body)))
    (tree-to-react (car body))))

(defpsmacro set-state% (&rest body)
  "Helper macro to set state"
  (if *with-self*
      `(chain self (set-state (create ,@body)))
      `(chain this (set-state (create ,@body)))))

(defun keyword-to-sym (kwd)
  (intern (symbol-name kwd) :react.paren))

(defun supported-tag-p (tag)
  (member (keyword-to-sym tag) *supported-react-tags* :test #'equal))

(defun process-body (sexp body-fn)
  (mapcan #'(lambda (def)
              (cond ((stringp def) (list def))
                    ((and (listp def) (keywordp (car def))) (list (funcall body-fn def)))
                    ((listp def) `(,def))
                    (t (list def))))
          sexp))

(defun fixup-attributes (attrs)
  "this does some name fixing for who vs paren vs js object props
   class -> className
   some-prop -> someProp"
  (mapcar #'(lambda (attr)
              (if (keywordp attr)
                  (cond ((eq attr :class) 'class-name)
                        ((find #\- (symbol-name attr)) (keyword-to-sym attr))
                        (t attr))
                  attr))
          attrs))

(defun tree-to-react (sexp)
  (let (tag attr-list body)
    (cond
      ((atom (first sexp))
       (setf tag (first sexp))
       ;; collect attribute/value pairs into ATTR-LIST and tag body (if
       ;; any) into BODY
       (loop for rest on (cdr sexp) by #'cddr
             if (keywordp (first rest))
               collect (list (first rest) (second rest)) into attr
             else
               do (progn (setf attr-list (apply #'nconc attr))
                         (setf body rest)
                         (return))
             finally (setf attr-list (apply #'nconc attr))))
      ((listp (first sexp))
       (setf tag (first (first sexp)))
       (loop for rest on (cdr (first sexp)) by #'cddr
          if (keywordp (first rest))
            collect (list (first rest) (second rest)) into attr
          finally (setf attr-list (apply #'nconc attr)))
       (setf body (cdr sexp))))
    (when attr-list
      (setf attr-list (fixup-attributes attr-list)))
    (if (supported-tag-p tag)
        `(chain ,*react-name* *d-o-m (,(keyword-to-sym tag) (create ,@attr-list) ,@(process-body body #'tree-to-react)))
        `(,(keyword-to-sym tag) (create ,@attr-list) ,@(process-body body #'tree-to-react)))))
