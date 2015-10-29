# React manual

###### \[in package REACT.PAREN\]
## react.paren ASDF System Details

- Version: 0.0.2
- Description: Bindings to react.js library with several extensions.
- Licence: The MIT License (MIT)
- Author: Crackbot <thecrackbot@gmail.com>
- Maintainer: Crackbot <thecrackbot@gmail.com>

This library defines couple of helper macros to make it easier to
use react.js library

- [variable] *WITH-SELF* T

    Define self variable inside each function binded to this: var self
    = this;

- [psmacro] COMPONENT 

- [psmacro] DEFCOMPONENT 

- [psmacro] RENDER-COMPONENT 

- [psmacro] WHO 

- [psmacro] SET-STATE% 

Example

```commonlisp
(defcomponent hello
  (defun render ()
    (who (:div (:p (@ this props text))))))

(render-component hello (-> document (get-element-by-id "test")))
```

