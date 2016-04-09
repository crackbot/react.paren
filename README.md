<a id='x-28SERVE-2EPAREN-3A-40MAIN-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# React manual

## Table of Contents

- [1 react.paren ASDF System Details][46e5]

###### \[in package REACT.PAREN\]
<a id='x-28-22react-2Eparen-22-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 react.paren ASDF System Details

- Version: 0.0.3
- Description: Bindings to react.js library with several extensions.
- Licence: The MIT License (MIT)
- Author: Crackbot <thecrackbot@gmail.com>
- Maintainer: Crackbot <thecrackbot@gmail.com>

This library defines couple of helper macros to make it easier to
use react.js library

<a id='x-28REACT-2EPAREN-3A-2AWITH-SELF-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*WITH-SELF\*** *T*

    Define self variable inside each function in component

<a id='x-28REACT-2EPAREN-3ACOMPONENT-20-28PSMACRO-29-29'></a>

- [psmacro] **COMPONENT** *NAME &REST BODY* 

    Define react component

<a id='x-28REACT-2EPAREN-3ADEFCOMPONENT-20-28PSMACRO-29-29'></a>

- [psmacro] **DEFCOMPONENT** *NAME &REST BODY* 

    Bind component to a variable of the same name as component

<a id='x-28REACT-2EPAREN-3ARENDER-COMPONENT-20-28PSMACRO-29-29'></a>

- [psmacro] **RENDER-COMPONENT** *NAME DOM* 

    Render component inside dom node

<a id='x-28REACT-2EPAREN-3AWHO-20-28PSMACRO-29-29'></a>

- [psmacro] **WHO** *&REST BODY* 

    Transform cl-who like forms into react nodes

<a id='x-28REACT-2EPAREN-3ASET-STATE-25-20-28PSMACRO-29-29'></a>

- [psmacro] **SET-STATE%** *&REST BODY* 

    Helper macro to set state

Example

```commonlisp

(defcomponent child
  (defun render ()
    (who (:p "Got prop: " (@ this props text)))))

(defcomponent hello
  (defun render ()
    (who (:div (:p (@ this props text)) (% child :text "it works!")))))

(render-component hello (-> document (get-element-by-id "test")))
```


  [46e5]: #x-28-22react-2Eparen-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"react.paren\" ASDF/SYSTEM:SYSTEM)"
