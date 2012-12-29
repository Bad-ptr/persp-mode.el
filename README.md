# Intro
Perspectives for emacs, based on perspective-mode
by Nathan Weizenbaum(http://github.com/nex3/perspective-el).  
But perspectives shared between frames
\+ ability to save/restore window configurations, save/restore from/to file.  

# Installation
Put this file into your `load-path`,
add `(require 'persp-mode) (persp-mode t)` into your ~/.emacs.  

# Keys
`C-x x s` -- create/switch to perspective.  
`C-x x r` -- rename perspective.  
`C-x x c` -- kill perspective.  
`C-x x a` -- add buffer to perspective.  
`C-x x i` -- import all buffers from another perspective.  
`C-x x k` -- remove buffer from perspective.  

# Customization
`M-x: customize-group RET persp-mode RET`  

---

# Save/load perspectives to/from file and auto save/resume

## Interactive functions:
`M-x: persp-save-state-to-file` and `M-x: load-state-from-file`.  
*Key bindings*: `C-x x w` and `C-x x l` accordingly.  

## Dependencies and troubles:
To be able to save/load from/to file, you must put my version of [`pickel.el`](https://github.com/Bad-ptr/pickel.el)
to your emacs load path.
To be able to save/restore window configurations you need [`workgroups.el`](https://github.com/tlh/workgroups.el).
It's also available on melpa.  
Also when you create new frame(with `emacsclient -c` for example)
it's window is switching to `*scratch*` buffer. To fix this you must have emacs 24.4 or build from bzr trunk.
Alternatively you can save `server.el` from `/usr/share/emacs/${your_emacs_version_number}/lisp/`
(or from source tree, or from somewhere else) to directory in your `load-path` and edit it like that:  
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

and edit `persp-set-ibc-to-f-is-supported` in `persp-mode.el`:  

    (defsubst persp-set-ibc-to-f-is-supported ()
      t)

## Variables:
Variable `persp-conf-dir` sets the directory where to save/load perspectives. By deafault it's set to `"~/.emacs.d/persp-confs"`.  
Variable `persp-auto-save-fname` sets the file name for auto-saving/-resuming. Default value is `"persp-auto-save"`.  
Variable `persp-auto-save-opt` (default is 2):  
    "0 -- do not auto save  
     1 -- save on exit and only if persp-mode active  
     2 -- save on persp-mode deactivation or at emacs exiting(if persp-mode is active)"  
Variable `persp-auto-resume` (default is t). If non nil persp will be restored from autosave file on mode activation.  
