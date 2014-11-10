# persp-mode  

## Intro  
Perspectives for emacs, based on the [`perspective-el`](http://github.com/nex3/perspective-el) by Nathan Weizenbaum.  
But perspectives are shared among frames \+ ability to save/restore perspectives' state from/to file.  

## Installation  
persp-mode is available from [`MELPA`](https://github.com/milkypostman/melpa). So if you use this repo then installation is easy as:  
`M-x: package-install RET persp-mode RET`  
Alternatively you can download the persp-mode.el from [`github`](https://github.com/Bad-ptr/persp-mode.el) and install it with:  
`M-x: package-install-file RET 'path_to_where_you_saved_persp-mode.el' RET`  

Another(oldschool;p) way:  
Put the persp-mode.el file somewhere in the emacs' load-path.  

### Suggested configuration  
It is recommended to switch off animation of restoring window configuration with `workgroups.el`.  
(it's clashing with the `golden-ration-mode` for example, sometimes erring when creating new frame
and it is slow on remote network connections.)  
You can do it with: `(setq wg-morph-on nil)`.  

#### When installing from MELPA  
```lisp
(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil) ;; switch off animation of restoring window configuration
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
```

#### When installing without generation of autoloads  
```lisp
(with-eval-after-load "persp-mode"
  (setq wg-morph-on nil)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
(require 'persp-mode)
```

On emacs <= 24.3 the macro `with-eval-after-load` is not defined. Here is how you can fix it -- add this to your .emacs(or .emacs.d/init.el or another init file that loads before persp-mode):  
```lisp
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1) (debug t))
    `(eval-after-load ,file '(progn ,@body))))
```


### Dependencies  
Ability to save/restore window configurations from/to file for emacs versions < 24.4 depends on [`workgroups.el`](https://github.com/tlh/workgroups.el) which also available from [`MELPA`](https://github.com/milkypostman/melpa).  

## Keys  
`s` -- create/switch to perspective.  
`r` -- rename perspective.  
`c` -- kill perspective. (if you try to kill 'none' persp -- it'l kill all opened buffers).  
`a` -- add buffer to perspective.  
`t` -- switch to buffer without adding it to the current perspective.  
`i` -- import all buffers from the another perspective.  
`k` -- remove buffer from perspective.  
`w` -- save perspectives to file.  
`l` -- load perspectives from file.  
  
These key sequences must follow the `persp-keymap-prefix` which you can customize(by default it is `C-c p` in older releases it was `C-x x`), so if you want to invoke the \< `s` - create/switch perspective \> you must first type prefix(`C-c p`) and then `s`(full sequence is `C-c p s`).  
If you want to bind new key for persp-mode, use `persp-key-map`:  
`(define-key persp-key-map (kbd ...) ...)`.  
  
If you kill buffer with `C-x k` it will be killed only if it belongs to a single perspective, otherwise it's just removed from current perspective.  
But if you kill buffer from 'none'(nil or main) perspective -- it will be removed from all perspectives and then killed.  

## Customization  
`M-x: customize-group RET persp-mode RET`  


## Interaction with side packages  

### Speedbar  
```lisp
(add-to-list 'speedbar-frame-parameters (cons 'persp-ignore-wconf t))
```

### Buffer lists

#### Universal
This must work for most buffer listing commands that use buffer-list function, just wrap 'your function' in `with-persp-buffer-list`:  
```lisp
(with-persp-buffer-list () (your-function))
```

##### bs-show  
```lisp
(global-set-key (kbd "C-x b") #'(lambda (arg)
                                  (interactive "P")
                                  (with-persp-buffer-list () (bs-show arg))))
```

##### ibuffer  
```lisp
(global-set-key (kbd "C-x b") #'(lambda (arg)
                                  (interactive "P")
                                  (with-persp-buffer-list () (ibuffer))))
```

And here is something ibuffer-specific: [gist](https://gist.github.com/Bad-ptr/7644606).


---

## Troubles  
If you updated or changed or simply something goes wrong don't warry to lose/overwrite perspectives' state, remember that persp-mode makes backups in `persp-save-dir' for you(3 previous states by default).  

When you create new frame(with `emacsclient -c` for example)
the selected window of created frame is switching to `*scratch*` buffer. This behaviour fixed in emacs version >= 24.4(and in current emacs trunk).
Alternatively you can save `server.el` from `/usr/share/emacs/${your_emacs_version_number}/lisp/`
(or from source tree, or from somewhere else) to directory in your `load-path` and edit it like that(this works for emacs 24.3 at least):  
replace  
```lisp
(unless (or files commands)
  (if (stringp initial-buffer-choice)
      (find-file initial-buffer-choice)
    (switch-to-buffer (get-buffer-create "*scratch*")
                      'norecord)))
```

by  

```lisp
(unless (or files commands)
  (let ((buf
         (cond ((stringp initial-buffer-choice)
                (find-file-noselect initial-buffer-choice))
               ((functionp initial-buffer-choice)
                (funcall initial-buffer-choice)))))
    (switch-to-buffer
     (if (buffer-live-p buf) buf (get-buffer-create "*scratch*"))
     'norecord)))
```
and set variable `persp-is-ibc-as-f-supported` to `t`.  
