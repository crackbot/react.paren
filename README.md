<a id='x-28SERVE-2EPAREN-3A-40MAIN-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# React manual

## Table of Contents

- [1 react.paren ASDF System Details][46e5]
- [2 Tutorial][3fa1]

###### \[in package REACT.PAREN\]
<a id='x-28-22react-2Eparen-22-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 react.paren ASDF System Details

- Version: 0.0.4
- Description: Bindings to react.js library with several extensions.
- Licence: The MIT License (MIT)
- Author: Crackbot <thecrackbot@gmail.com>
- Maintainer: Crackbot <thecrackbot@gmail.com>

This library defines couple of helper macros to make it easier to
use react.js library

<a id='x-28REACT-2EPAREN-3A-2AWITH-SELF-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*WITH-SELF\*** *T*

    Define self variable inside each function in component

<a id='x-28REACT-2EPAREN-3ACOMPONENT-20-28MGL-PAX-EXT-3APSMACRO-29-29'></a>

- [psmacro] **COMPONENT** *NAME &REST BODY* 

    Create new react class component. Each body definition should
    either be a `DEFUN` form or an extension. Extension is defined to be a
    form that starts with keyword. Any other for will be ignored. Here is an example:
    
    ```lisp
    (component *test
      (:>> on-click functionp)
               
      (defun on-click (ev)
        (-> this props (on-click)))
    
      (defun render ()
        (who (:button :on-click (@ this on-click)))))
    ```
    
    There is only one supported extension right now `:>>` which acts as
    contract validator for react props. And supports all contracts.paren
    contracts.
    
    This follows the same semantics as DEFJSCLASS.

<a id='x-28REACT-2EPAREN-3ADEFCOMPONENT-20-28MGL-PAX-EXT-3APSMACRO-29-29'></a>

- [psmacro] **DEFCOMPONENT** *NAME &REST BODY* 

    Bind component to a variable of the same name as component. In most
    cases you want to use that instead of [`COMPONENT`][b967] directly

<a id='x-28REACT-2EPAREN-3ARENDER-COMPONENT-20-28MGL-PAX-EXT-3APSMACRO-29-29'></a>

- [psmacro] **RENDER-COMPONENT** *NAME DOM* 

    Render component inside dom node, initializing the component

<a id='x-28REACT-2EPAREN-3ARENDER-20-28MGL-PAX-EXT-3APSMACRO-29-29'></a>

- [psmacro] **RENDER** *NAME DOM* 

    Render component inside dom node. You need to do component
    initialization yourself, for that you can use `CREATE-EL`
    macro. Example:
    
    ```lisp
    (render (create-el *todo-app :data data)
            (-> document (get-element-by-id "test")))
    ```


<a id='x-28REACT-2EPAREN-3AWHO-20-28MGL-PAX-EXT-3APSMACRO-29-29'></a>

- [psmacro] **WHO** *&REST BODY* 

    Transform cl-who like forms into react nodes. In most cases you
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
      dynamic, use `CREATE-EL`.

<a id='x-28REACT-2EPAREN-3ASET-STATE-25-20-28MGL-PAX-EXT-3APSMACRO-29-29'></a>

- [psmacro] **SET-STATE%** *&REST BODY* 

    Helper macro to set state

<a id='x-28REACT-2EPAREN-3A-40REACT-TUTORIAL-20MGL-PAX-3ASECTION-29'></a>

## 2 Tutorial

Few simple components:

```lisp

   (defcomponent child
     (defun render ()
       (who (:p "Got prop: " (@ this props text)))))
   
   (defcomponent hello
     (defun render ()
       (who (:div (:p (@ this props text))
                  (% child :text "it works!")))))

   (render-component hello (-> document (get-element-by-id "test")))
```


Here is comment form translated from
react.js (tutorial)[https://facebook.github.io/react/docs/tutorial.html][]
With the only difference of contract check.

```lisp
(defcomponent *comment-form
  (:>> on-comment-submit functionp)
  
  (defun get-initial-state ()
    (create :author ""
            :text ""))

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
        (set-state% :author ""
                    :text ""))))

  (defun render ()
    (who (:form :class "commentForm" :on-submit (@ this handle-submit)
                (:input :type "text"
                        :placeholder "Your name"
                        :value (@ this state author)
                        :on-change (@ this handle-author-change))
                (:input :type "text"
                        :palceholder "Say something..."
                        :value (@ this state text)
                        :on-change (@ this handle-text-change))
                (:input :type "submit"
                        :value "Post")))))
```


  [3fa1]: #x-28REACT-2EPAREN-3A-40REACT-TUTORIAL-20MGL-PAX-3ASECTION-29 "Tutorial"
  [46e5]: #x-28-22react-2Eparen-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"react.paren\" ASDF/SYSTEM:SYSTEM)"
  [b967]: #x-28REACT-2EPAREN-3ACOMPONENT-20-28MGL-PAX-EXT-3APSMACRO-29-29 "(REACT.PAREN:COMPONENT (MGL-PAX-EXT:PSMACRO))"
