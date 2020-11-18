# cursor-undo.el
Undo cursor position on each el-screen

## Configure example

```emacs-lisp
(require 'cursor-undo)
(cursor-undo-init)
(global-set-key (kbd "M-r") 'cursor-undo-undo-position)
```

