
(in-package :react.paren)

(defpssyslib "react" :react.paren)

(defsection @main-manual (:title "React manual")
  (react.paren asdf:system)
  
  "This library defines couple of helper macros to make it easier to
use react.js library"
  (*with-self* variable)

  (component psmacro)
  (defcomponent psmacro)
  (render-component psmacro)
  (render psmacro)
  (who psmacro)
  (set-state% psmacro)
  (@react-tutorial section))

(defsection @react-tutorial (:title "Tutorial")
  "Few simple components:

```lisp
(defcomponent child
  (defun render ()
    (who (:p \"Got prop: \" (@ this props text)))))
   
(defcomponent hello
  (defun render ()
    (who (:div (:p (@ this props text))
               (% child :text \"it works!\")))))

(render-component hello (-> document (get-element-by-id \"test\")))
```
"

  "Here is comment form translated from
react.js [tutorial](https://facebook.github.io/react/docs/tutorial.html)
With the only difference of contract check.

```lisp
(defcomponent *comment-form
  (:>> on-comment-submit functionp)
  
  (defun get-initial-state ()
    (create :author \"\"
            :text \"\"))

  (defun handle-author-change (e)
    (set-state% :author (@ e target value)))

  (defun handle-text-change (e)
    (set-state% :text (@ e target value)))

  (defun handle-submit (e)
    (-> e (prevent-default))
    (let ((author (-> this state author (trim)))
          (text   (-> this state text (trim))))
      (when (and author text)
        (-> this props (on-comment-submit (create :author author
                                                  :text   text)))
        (set-state% :author \"\"
                    :text \"\"))))

  (defun render ()
    (who (:form :class \"commentForm\" :on-submit (@ this handle-submit)
                (:input :type \"text\"
                        :placeholder \"Your name\"
                        :value (@ this state author)
                        :on-change (@ this handle-author-change))
                (:input :type \"text\"
                        :palceholder \"Say something...\"
                        :value (@ this state text)
                        :on-change (@ this handle-text-change))
                (:input :type \"submit\"
                        :value \"Post\")))))
```
")

(defparameter *react-name* '*react)
(defparameter *react-dom* '*react-d-o-m)

(defparameter *with-self* t
  "Define self variable inside each function in component")

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
  "React defun forms support contracts, this function transforms defun
form into corresponding lambda form"
  ;'lambda/contract)
  (case defun-form
    (defun 'lambda)
    (defun/contract 'lambda/contract)))

(defun process-props (component-name props-contracts)
  "Instead of using standard react.js prop validators, this library
integrates with contracts.paren. You can use contracts to test
props. Only single combinator is supported right now: >>"
  (let ((res '()))
    (do ((i 0 (+ i 2)))
        ((>= i (length props-contracts)))
      (progn
        (let ((expected (with-output-to-string (out)
                          (format out "~A" (nth (+ i 1) props-contracts)))))
          (push (fixup-attr (nth i props-contracts)) res)
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

(defpsmacro component (name &rest body)
  "Create new react class component. Each body definition should
either be a DEFUN form or an extension. Extension is defined to be a
form that starts with keyword. Any other for will be ignored. Here is an example:

```lisp
(component *test
  (:>> on-click functionp)
           
  (defun on-click (ev)
    (-> this props (on-click)))

  (defun render ()
    (who (:button :on-click (@ this on-click)))))
```

There is only one supported extension right now :>> which acts as
contract validator for react props. And supports all contracts.paren
contracts.

This follows the same semantics as DEFJSCLASS."
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
  "Bind component to a variable of the same name as component. In most
  cases you want to use that instead of COMPONENT directly"
  `(var ,name (component ,name ,@body)))

(defpsmacro render-component (name dom)
  "Render component inside dom node, initializing the component"
  `(chain ,*react-dom* (render (chain ,*react-name* (create-element ,name nil)) ,dom)))

(defpsmacro render (name dom)
  "Render component inside dom node. You need to do component
initialization yourself, for that you can use CREATE-EL
macro. Example:

```lisp
(render (create-el *todo-app :data data)
        (-> document (get-element-by-id \"test\")))
```
"

  `(chain ,*react-dom* (render ,name ,dom)))

(defpsmacro who (&rest body)
  "Transform cl-who like forms into react nodes. In most cases you
  need to use that function in component's render method. For
  initialization of other components you need to use special form
  starting with %. Example:

```lisp
(who (% todo-list :item (@ this state items)))
```
 
  You can also use this form to initialize components inside
  functions. Example:

```lisp
(defcomponent todo-list
  (defun init-item (data)
    (who (% todo-item :data data)))
    
  (defun render ()
    (let ((items (mapcar (@ this init-item)
                         (@ this state items))))
      (who (:ul items))))
```

  However if you need to initialize component dynamically, when
  component is referenced by a variable or it's parameters are
  dynamic, use CREATE-EL.
"
  (when (and (listp body) (listp (car body)))
    (tree-to-react (car body))))

(defpsmacro set-state% (&rest body)
  "Helper macro to set state"
  (if *with-self*
      `(chain self (set-state (create ,@body)))
      `(chain this (set-state (create ,@body)))))

(defpsmacro create-el (tag &rest sexp)
  "Create new element. This acts the same as special % form inside
WHO. The difference is in how body is processed. Use this form when
you have dynamic component/attributes. Or when initializing new
component outside of any other component. Example:

```lisp
(let ((component (get-some-component))
      (data (create :key 1
                    :args (get-some-args))))
  (create-el component data))
```
"
  `(chain ,*react-name* (create-element ,tag
                                        ,@sexp)))

(defun keyword-to-sym (kwd)
  (intern (symbol-name kwd) :react.paren))

;; (defun keyword-to-str (kwd)
;;   (keyword-to-sym

(defun supported-tag-p (tag)
  (member (keyword-to-sym tag) *supported-react-tags* :test #'equal))

(defun fixup-attr (attr)
  "Given attr it will transform it into parenscript form that
translates into camelCase"
  (if (keywordp attr)
      (cond ((eq attr :class) 'class-name)
            ((find #\- (symbol-name attr)) (keyword-to-sym attr))
            (t attr))
      attr))

(defun fixup-attrs (attrs)
  "this does some name fixing for who vs paren vs js object props
   class -> className
   some-prop -> someProp"
  (mapcar #'fixup-attr attrs))

(defun process-body (sexp body-fn)
  (mapcan #'(lambda (def)
              (cond ((stringp def) (list def))
                    ((and (listp def) (keywordp (car def))) (list (funcall body-fn def)))
                    ((listp def) (list (funcall body-fn def)))
                    (t (list def))))
          sexp))

(defun tree-to-react (sexp)
  "Convert sexp tree to react components."
  (let (comp-init tag attr-list body)
    (when (eql (car sexp) '%)
      (setf comp-init t
            sexp (cdr sexp)))
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
      (setf attr-list (fixup-attrs attr-list)))

    ;(break)
    (cond (comp-init
           `(chain ,*react-name* (create-element ,(keyword-to-sym tag)
                                                 (create ,@attr-list)
                                                 ,@(process-body body #'tree-to-react))))
          ((supported-tag-p tag)
           `(chain ,*react-name* (create-element ,(string-downcase (symbol-name tag))
                                                 (create ,@attr-list)
                                                 ,@(process-body body #'tree-to-react))))
          (t `(,@sexp)))))
