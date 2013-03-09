# persp-mode

## Intro
Perspectives for emacs, based on [`perspective-mode`](http://github.com/nex3/perspective-el) by Nathan Weizenbaum.  
But perspectives shared between frames \+ ability to save/restore window configurations, save/restore from/to file.  

## Installation
persp-mode is available on [`melpa`](https://github.com/milkypostman/melpa). So if you use this repo installation is easy:  
`M-x: package-install RET persp-mode RET`  
Alternatively you can download persp-mode.el from github and install it with:  
`M-x: package-install-file RET 'path_to_where_you_saved_persp-mode.el' RET`  

Another(oldschool;p) way:  
Put persp-mode.el file somewhere in your emacs load-path and add `(require 'persp-mode) (persp-mode t)` into your ~/.emacs.  

### Dependencies:
Ability of saving/restoring window configurations from/to file depends on [`workgroups.el`](https://github.com/tlh/workgroups.el).  
It's automatically installed if you install persp-mode from mepla, otherwise you must download it and put somewhere in your emacs load-path.  

## Keys
`C-x x s` -- create/switch to perspective.  
`C-x x r` -- rename perspective.  
`C-x x c` -- kill perspective. (if you try to kill 'none' persp -- it'l kill all opend buffers).  
`C-x x a` -- add buffer to perspective.  
`C-x x i` -- import all buffers from another perspective.  
`C-x x k` -- remove buffer from perspective.  
`C-x x w` -- save perspectives to file.  
`C-x x l` -- load perspectives from file.  

## Customization
`M-x: customize-group RET persp-mode RET`  

---

## Troubles:
When you create new frame(with `emacsclient -c` for example)
it's window is switching to `*scratch*` buffer. This behaviour fixed in emacs version >= 24.4.
Alternatively you can save `server.el` from `/usr/share/emacs/${your_emacs_version_number}/lisp/`
(or from source tree, or from somewhere else) to directory in your `load-path` and edit it like that(this works for emacs 24.3 at least):  
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

and set variable `persp-is-ibc-as-f-supported` to `t`.
