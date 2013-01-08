# Intro
Perspectives for emacs, based on [perspective-mode](http://github.com/nex3/perspective-el) by Nathan Weizenbaum.  
But perspectives shared between frames \+ ability to save/restore window configurations, save/restore from/to file.  

# Installation
Put this file into your `load-path`,
add `(require 'persp-mode) (persp-mode t)` into your ~/.emacs.  
I also planing to add this package to [melpa](https://github.com/milkypostman/melpa) soon.  

## Dependencies:
To be able to save/restore window configurations from/to file you need [`workgroups.el`](https://github.com/tlh/workgroups.el).
It's also available on [melpa](https://github.com/milkypostman/melpa) (`M-x: package-install RET workgroups RET`).  

# Keys
`C-x x s` -- create/switch to perspective.  
`C-x x r` -- rename perspective.  
`C-x x c` -- kill perspective.  
`C-x x a` -- add buffer to perspective.  
`C-x x i` -- import all buffers from another perspective.  
`C-x x k` -- remove buffer from perspective.  
`C-x x w` -- save perspectives to file.  
`C-x x l` -- load perspectives from file.  

# Customization
`M-x: customize-group RET persp-mode RET`  

---

# Troubles:
When you create new frame(with `emacsclient -c` for example)
it's window is switching to `*scratch*` buffer. To fix this you must have emacs version >= 24.4.
Alternatively you can save `server.el` from `/usr/share/emacs/${your_emacs_version_number}/lisp/`
(or from source tree, or from somewhere else) to directory in your `load-path` and edit it like that(that works for emacs 24.3 at least):  
replace  

    (unless (or files commands)
             (if (stringp initial-buffer-choice)
             (find-file initial-buffer-choice)
           	(switch-to-buffer (get-buffer-create "*scratch*")
           		  'norecord)))

by  

    (unless (or files commands)
      (let ((buf
        	 (cond ((stringp initial-buffer-choice)
      (find-file-noselect initial-buffer-choice))
     ((functionp initial-buffer-choice)
      (funcall initial-buffer-choice)))))
    (switch-to-buffer
     (if (buffer-live-p buf) buf (get-buffer-create "*scratch*"))
     'norecord)))

and edit `persp-is-ibc-as-f-supported` in `persp-mode.el`:  

    (defsubst persp-is-ibc-as-f-supported ()
      t)
