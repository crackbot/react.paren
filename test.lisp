
(in-package :react.paren-tests)

(defmacro assert-equal-str (form1 form2)
  `(assert-equal (remove-ws ,form1)
                 (remove-ws ,form2)))

(defun remove-ws (str)
  "Removes all whitespace from string"
  (remove #\return (remove #\  str)))

(define-test test-defcomponent-minimal
  (assert-equal-str (ps (defcomponent hello))
                    "React.createClass({ 'displayName' : 'HELLO' });"))

(define-test test-defcomponent-full
  (assert-equal-str (ps (defcomponent hello
                      (fun (x) (+ x x))))
                "React.createClass({ 'displayName' : 'HELLO', fun : function (x) {
                     return x + x;
                 } });"))

(define-test test-defcomponent!
  (assert-equal-str (ps (defcomponent! hello))
                    "var hello = React.createClass({ 'displayName' : 'HELLO' });"))

(define-test test-render-component
  (assert-equal-str (ps (render-component hello node))
                    "React.renderComponent(hello, node);"))

(define-test test-who->react-simple
  (assert-equal-str (ps (who->react (:div "Text")))
                    "React.DOM.div({  }, 'Text');"))

(define-test test-who->react-full
  (assert-equal-str (ps (who->react (:div :class "value" :another-property "value"
                                          (:p "Text"))))
                    "React.DOM.div({ className : 'value', anotherProperty : 'value' }, React.DOM.p({  }, 'Text'));"))

(define-test test-who-non-tag
  (assert-equal-str (ps (who->react (:div (:some-fun "Text"))))
                    "React.DOM.div({  }, someFun({  }, 'Text'));")
  (assert-equal-str (ps (who->react (:div (some-fun "Text"))))
                    "React.DOM.div({  }, someFun('Text'));"))

(define-test test-component-with-who
  (assert-equal-str (ps (defcomponent hello
                          (render ()
                            (who->react (:div "Text")))))
                    "React.createClass({ 'displayName' : 'HELLO', render : function () {
                         return React.DOM.div({  }, 'Text');
                     } });"))

(run-tests :all)
