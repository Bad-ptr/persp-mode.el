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
`S` -- create/switch to perspective in a window.  
`r` -- rename perspective.  
`c` -- kill perspective. (if you try to kill 'none' persp -- it'l kill all opened buffers).  
`a` -- add buffer to perspective.  
`t` -- switch to buffer without adding it to the current perspective.  
`i` -- import all buffers from another perspective.  
`k` -- remove buffer from perspective.  
`K` -- kill buffer.  
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

## switch-to-buffer, display-buffer hook, and other advices:  

Some time ago there were switch-to-buffer and display-buffer advices in the persp-mode.
If you still need them, I can suggest you the way:

```lisp
(with-eval-after-load "persp-mode"
  (defvar after-switch-to-buffer-functions nil)
  (defvar after-display-buffer-functions nil)

  (if (fboundp 'advice-add)
      ;;Modern way
      (progn
        (defun after-switch-to-buffer-adv (&rest r)
          (apply #'run-hook-with-args 'after-switch-to-buffer-functions r))
        (defun after-display-buffer-adv (&rest r)
          (apply #'run-hook-with-args 'after-display-buffer-functions r))
        (advice-add #'switch-to-buffer :after #'after-switch-to-buffer-adv)
        (advice-add #'display-buffer   :after #'after-display-buffer-adv))

    ;;Old way
    (defadvice switch-to-buffer (after after-switch-to-buffer-adv)
      (run-hook-with-args 'after-switch-to-buffer-functions (ad-get-arg 0)))
    (defadvice display-buffer (after after-display-buffer-adv)
      (run-hook-with-args 'after-display-buffer-functions (ad-get-arg 0)))
    (ad-enable-advice #'switch-to-buffer 'after 'after-switch-to-buffer-adv)
    (ad-enable-advice #'display-buffer 'after 'after-display-buffer-adv)
    (ad-activate #'switch-to-buffer)
    (ad-activate #'display-buffer)))
```

After that you can add functions to `after-switch-to-buffer-functions` and `after-display-buffer-functions`:

```lisp
(add-hook 'after-switch-to-buffer-functions
    #'(lambda (bn) (when (and persp-mode
                              (not persp-temporarily-display-buffer))
                     (persp-add-buffer bn))))
```

## Auto perpectives  

You can now define an auto perspective using the `def-auto-persp` macro.  
This kind of perspectives is intended to be dynamically created/destroyed/hided/unhided when a specific kind of buffers appears/disappiars.  

The argument list of the `def-auto-persp` macro:  

The first argument is a string which will serve as a name for the auto perspective.  

Other arguments is a key - value pairs:  

`:buffer-name` -- regexp to match agains a name of a buffer.  
`:file-name` -- regexp to match against a filename of the buffer.  
`:mode` -- symbol to compare with the major-mode of the buffer.  
`:mode-name` -- regexp to compare against mode-name of the buffer.  
`:predicate` -- function to check if the buffer is a good one(return nil if not).  
`:on-match` -- function to run when the buffer passed all checks, instead of standard actions(create/get perspective, add buffer to it).  
  arguments passed to that function:  
  1. the name of perspective;  
  2. the buffer;  
  3. `after-match` argument;  
  4. the hook that was triggered;  
  5. arguments passed to that hook.  

`:after-match` -- function to run after the buffer match and standard or custom action.  
  arguments passed to that function:  
  1. the perspective  
  2. the buffer  
  3. the hook  
  4. the hook arguments  

`:hooks` -- the list of hooks (or symbol) to which you want to add checks.  
  `def-auto-persp` tries to be smart about hooks to which it'll add checks, but sometimes you need more control.  
`:dyn-env` -- the list of variables and values to dynamically bind when the checks and action takes place. The format is the same as in the `let` form.  
`:get-name-expr` -- expression to get a perspecive name.  
`:get-buffer-expr` -- expression to get the buffer.  
`:get-persp-expr` -- expression to get the perspective.  
`:parametes` -- list of parameters for perspective(see the `modify-persp-parameters` function).  
`:noauto` -- if non nil then do not set the auto field of the perspective.  

Only the name string(first argument) is required. All other arguments could be ommited or combined in any way you like.  

Example of usage:
```lisp
(with-eval-after-load "persp-mode-autoload"
  (with-eval-after-load "dired"
    (def-auto-persp "dired"
      :parameters '((dont-save-to-file . t))
      :mode dired-mode
      :dyn-env (after-switch-to-buffer-functions ;; prevent recursion
                (persp-add-buffer-on-find-file nil)
                persp-add-buffer-on-after-change-major-mode)
      :hooks (after-switch-to-buffer-functions)
      :after-match #'(lambda (p b h ha)
                       (persp-window-switch (safe-persp-name p))))))
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
