# Intro
Perspectives for emacs, based on perspective-mode by Nathan Weizenbaum(http://github.com/nex3/perspective-el)
but perspectives shared between frames + some fixes and workarounds

# Howto
Put somwhere in emacs load path, then `(require 'persp-mode) (persp-mode t)`

# Keys
`C-x x s` create/switch to persp.  
`C-x x r` rename persp.  
`C-x x c` kill persp.  
`C-x x a` add buffer to persp.  
`C-x x i` import all buffers from other persp.  
`C-x x k` remove buffer from persp.  


# Save/load perspectives to/from file and auto save/resume

## Dependencies:
To be able to save/load persps, you must put my version of `pickel.el`(experimental branch) to your emacs load path. //(original https://github.com/tlh/pickel.el)  
To be able to save/restore also persp's window configurations you need `workgroups.el`(https://github.com/tlh/workgroups.el). It's also available on melpa.  

## Interactive functions:
`M-x: persp-save-state-to-file` and `M-x: load-state-from-file`.

## Variables:
Variable `persp-conf-dir` sets the directory where to save/load perspectives. By deafault it's set to `"~/.emacs.d/persp-confs"`.  
Variable `persp-auto-save-fname` sets the file name for auto-saving/-resuming. Default value is `"persp-auto-save"`.  
Variable `persp-auto-save-opt` (default is 2):  
    "0 -- do not auto save  
     1 -- save on exit and only if persp-mode active  
     2 -- save on persp-mode deactivation or at emacs exiting(if persp-mode is active)"  
Variable `persp-auto-resume` (default is t). If non nil persp will be restored from autosave file on mode activation.  
