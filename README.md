# persp-mode  

## Intro  
Perspectives for emacs, based on the [`perspective-el`](http://github.com/nex3/perspective-el) by Nathan Weizenbaum.  
But the perspectives are shared among frames \+ ability to save/restore it's state from/to a file.  

## Installation  
The persp-mode is available from the [`MELPA`](https://github.com/milkypostman/melpa). So if you use this repo then the installation is easy:  
`M-x: package-install RET persp-mode RET`  
Alternatively you can download the persp-mode.el from [`github`](https://github.com/Bad-ptr/persp-mode.el) and install it as a package:  
`M-x: package-install-file RET 'path_to_where_you_saved_persp-mode.el' RET`  

Another(oldschool;p) way:  
Place the persp-mode.el file somewhere in the emacs' load-path and add `(require 'persp-mode) (persp-mode 1)` to your configuration file.  

### Suggested configuration  
If you use the [`workgroups.el`](https://github.com/tlh/workgroups.el) it is good idea to switch off the restore windows animation.  
(it's clashing with the [`golden-ration-mode`](https://github.com/roman/golden-ratio.el) for example, sometimes erring when creating new frames
and it is slow on remote network connections.)  
You can do it with: `(setq wg-morph-on nil)`.  

#### When installing from MELPA  
```lisp
(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil) ;; switch off animation
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
```

#### When installing without generation of autoloads  
```lisp
(with-eval-after-load "persp-mode"
  (setq wg-morph-on nil)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
(require 'persp-mode)
```

If you run emacs <= 24.3 the macro `with-eval-after-load` is not defined. You can fix it by adding to your .emacs(or .emacs.d/init.el or another init file that loads before the persp-mode) these lines:  
```lisp
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1) (debug t))
    `(eval-after-load ,file '(progn ,@body))))
```


### Dependencies  
Ability to save/restore window configurations from/to a file for emacs < 24.4 depends on the [`workgroups.el`](https://github.com/tlh/workgroups.el) which also available from [`MELPA`](https://github.com/milkypostman/melpa).  

## Keys  
`n` -- next perspective.  
`p` -- previous perspective.  
`s` -- create/switch to perspective.  
`r` -- rename perspective.  
`c` -- kill perspective. (if you try to kill 'none' persp -- it'l kill all opened buffers).  
`a` -- add buffer to perspective.  
`t` -- switch to buffer without adding it to the current perspective.  
`i` -- import all buffers from another perspective.  
`k` -- remove buffer from perspective.  
`w` -- save perspectives to file.  
`l` -- load perspectives from file.  
`o` -- switch off persp-mode.  (you can quickly switch off persp-mode after emacs start and before autoresuming previous perspectives state if you only need to edit a single file.)  
  
These key sequences must follow the `persp-keymap-prefix` which you can customize(by default it is `C-c p` in older releases it was `C-x x`), so if you want to invoke the \< `s` - create/switch perspective \> command you must first type the prefix(`C-c p`) and then `s`(full sequence is `C-c p s`).  
If you want to bind a new key for persp-mode, use `persp-key-map`:  
`(define-key persp-key-map (kbd ...) ...)`.  
  
If you kill a buffer with `C-x k`(kill-buffer command) it will be killed only if it belongs to a single perspective, otherwise it'l be only removed from the current perspective and not killed.  
But if you kill a buffer from the 'none'(nil) perspective -- it will be removed from all perspectives and then killed.  

## Customization  
`M-x: customize-group RET persp-mode RET`  

## Custom save/load buffer function example  
Suppose you want to save the `*ielm*`(M-x ielm RET -- elisp repl) buffers.  
Then the save function would be:
```lisp
(lambda (b)
  (with-current-buffer b
    (when (string= major-mode "inferior-emacs-lisp-mode")
      `(def-ielm-buffer ,(buffer-name) ,default-directory))))
```
You must prepend that function to the `persp-save-buffer-functions` list (before the standard filtering functions couse it filters buffers starting with the '*').  

The load function:
```lisp
(lambda (savelist)
  (when (eq (car savelist) 'def-ielm-buffer)
    (with-current-buffer (get-buffer-create (cadr savelist))
      (setq default-directory (caddr savelist))
      (require 'ielm)
      (inferior-emacs-lisp-mode))))
```
Add load function to the `persp-load-buffer-functions` list.  
That's it. Now the persp-mode could save and restore ielm buffers.  

Python shell example:
```lisp
(with-eval-after-load "persp-mode-autoloads"
  (add-to-list 'persp-save-buffer-functions
               #'(lambda (b)
                   (when (eq 'inferior-python-mode (buffer-local-value 'major-mode b))
                     `(def-inferior-python-buffer ,(buffer-name b)
                        ,(let ((process (get-buffer-process b)))
                           (if process
                               (progn
                                 (python-shell-send-string "import os" process)
                                 (python-shell-send-string-no-output "os.getcwd()" process))
                             (concat "'" (buffer-local-value 'default-directory b) "'")))))))
  (add-to-list 'persp-load-buffer-functions
               #'(lambda (savelist)
                   (when (eq (car savelist) 'def-inferior-python-buffer)
                     (destructuring-bind (bname dir) (cdr savelist)
                       (run-python nil nil nil)
                       (with-current-buffer (python-shell-get-buffer)
                         (rename-buffer bname)
                         (cd dir)
                         (python-shell-send-string "import os")
                         (python-shell-send-string (format "os.chdir(%s)" dir))
                         (current-buffer)))))))
```


## Interaction with side packages  

### Speedbar  
```lisp
(add-to-list 'speedbar-frame-parameters (cons 'persp-ignore-wconf t))
```

### Buffer lists  

#### Universal  
This must work for most buffer listing commands that internally use the `buffer-list` function, just wrap 'your function' with the `with-persp-buffer-list`:  
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
                                  (with-persp-buffer-list () (ibuffer arg))))
```

And here is something ibuffer-specific: [gist](https://gist.github.com/Bad-ptr/7644606).  

##### helm  
Buffer filtering support: [gist](https://gist.github.com/Bad-ptr/304ada85c9ba15013303).  
Also, you can take a look at [Spacemacs](https://github.com/syl20bnr/spacemacs), and especially [this](https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Bwindow-management/spacemacs-layouts/funcs.el).  


## Hints  
If you often launch emacs to edit a single file and you don't want to wait the persp-mode 
resuming process(and don't want to use the emacs daemon) -- you can create a script like that:
```shell
 #!/bin/bash
 emacs --eval '(setq persp-auto-resume-time -1.0 persp-auto-save-opt 0)' $@;
```
call it editor.sh, save somewhere in the $PATH, and add `export EDITOR="editor.sh"` to your .bashrc.  
Or add
```lisp
(add-to-list 'command-switch-alist
               (cons "persp-q"
                     #'(lambda (p)
                         (setq persp-auto-resume-time -1
                               persp-auto-save-opt 0))))
```
To your emacs config. Then the editor.sh would be:
```shell
 #!/bin/bash
 emacs -persp-q $@;
```

---

## Troubles  
If you updated or changed something or simply something goes wrong don't warry to lose/overwrite perspectives' state, remember that the persp-mode makes backups in `persp-save-dir' for you(3 previous states by default).  

When you create a new frame(with `emacsclient -c` for example)
the selected window of the created frame is switching to the `*scratch*` buffer. This behaviour is fixed in the emacs version >= 24.4(and in current emacs trunk).
Alternatively you can save the `server.el` from `/usr/share/emacs/${your_emacs_version_number}/lisp/`
(or from source tree, or from somewhere else) to a directory in your `load-path` and edit it like that(this works for emacs 24.3 at least):  
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
and set the `persp-is-ibc-as-f-supported` variable to `t`.  
