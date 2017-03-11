;;; persp-mode.el --- windows/buffers sets shared among frames + save/load.

;; Copyright (C) 2012 Constantin Kulikov

;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Version: 2.9.6
;; Package-Requires: ()
;; Keywords: perspectives, session, workspace, persistence, windows, buffers, convenience
;; URL: https://github.com/Bad-ptr/persp-mode.el

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Based on the perspective.el by Natalie Weizenbaum
;;  (http://github.com/nex3/perspective-el) but the perspectives are shared
;;   among the frames and could be saved/restored from/to a file.
;;
;; Homepage: https://github.com/Bad-ptr/persp-mode.el

;; Installation:

;; From the MELPA: M-x package-install RET persp-mode RET
;; From a file: M-x package-install-file RET 'path to this file' RET
;; Or put this file into your load-path.

;; Configuration:

;; When installed through the package-install:
;; (with-eval-after-load "persp-mode-autoloads"
;;   (setq wg-morph-on nil)
;;   ;; switch off the animation of restoring window configuration
;;   (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))

;; When installed without generating an autoloads file:
;; (with-eval-after-load "persp-mode"
;;   (setq wg-morph-on nil)
;;   (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
;; (require 'persp-mode)

;; Dependencies:

;; The ability to save/restore window configurations from/to a file
;;  depends on the workgroups.el(https://github.com/tlh/workgroups.el)
;;   for the emacs versions < 24.4

;; Keys:

;; n -- switch to next perspective.
;; p -- switch to previous perspective.
;; s -- create/switch to perspective in frame.
;; S -- create/switch to perspective in window.
;; r -- rename perspective.
;; c -- copy current perspective.
;; C -- kill perspective.
;;   Calling with prefix argument will not kill perspective's buffers
;;   (however if you try to kill 'none' persp -- it'l kill all opened buffers).
;; a -- add buffer to perspective.
;;   Calling with prefix argument reverses the effect of the persp-switch-to-added-buffer.
;; b -- switch to buffer in perspective.
;; t -- switch to buffer without adding it to current perspective.
;;   Calling with prefix argument allows to remove a buffer from perspective without
;;   killing and switching to another buffer.
;; i -- import all buffers from another perspective.
;; I -- import window configuration from another perspective.
;; k -- remove buffer from perspective.
;;   Calling with prefix argument reverses the effect of the persp-auto-kill-buffer-on-remove.
;; K -- kill buffer.
;; w -- save perspectives to file.
;; W -- save subset of perspectives to file.
;; l -- load perspectives from file.
;; L -- load subset of perspectives from file.
;; o -- switch off persp-mode.
;;   (This may be useful when you launch emacs just to edit a single file and don't want to
;; restore buffers)

;; These key sequences must follow the `persp-keymap-prefix' which you can customize
;;  (by default it is 'C-c p' in older releases it was 'C-x x')
;;   so if you want to invoke the < s - create/switch perspective > command
;;    you must first type the prefix ('C-c p') and then 's'(full sequence is C-c p s).
;;
;; If you want to bind a new key for persp-mode, use the `persp-key-map`:
;;  `(define-key persp-key-map (kbd ...) ...)`.

;; If you kill a buffer with the 'C-x k' it will be killed only if it belongs to
;;  a single perspective, otherwise it'l be just removed from the current perspective.
;; But if you kill a buffer from the 'none'(nil) perspective --
;;  it will be removed from all perspectives and then killed.


;; Customization:

;; M-x: customize-group RET persp-mode RET


;;; Code:


;; Prerequirements:

(require 'cl)
(require 'easymenu)

(defvar persp-mode nil)

(defconst persp-not-persp :nil
  "Something that is not a perspective.")

(unless (fboundp 'condition-case-unless-debug)
  (defalias 'condition-case-unless-debug 'condition-case-no-debug))
(unless (fboundp 'read-multiple-choice)
  (defun read-multiple-choice (prompt choices)
    (let ((choice-chars (mapcar #'car choices)))
      (when choice-chars
        (assq (read-char-choice
               (format "%s(%s): "
                       (substring prompt 0 (string-match ": $" prompt))
                       (mapconcat #'(lambda (ch)
                                      (format "[%c] - %s" (car ch) (cadr ch)))
                                  choices "; "))
               choice-chars)
              choices)))))
(unless (fboundp 'alist-get)
  (defun alist-get (key alist &optional default remove)
    (ignore remove) ;;Silence byte-compiler.
    (let ((x (assq key alist)))
      (if x (cdr x) default))))


;; Customization variables:

(unless
    (memq 'custom-group (symbol-plist 'session))
  (defgroup session nil
    "Emacs' state(opened files, buffers, windows, etc.)"
    :group 'environment))

(defgroup persp-mode nil
  "Customization of the `persp-mode'."
  :prefix "persp-"
  :group 'session
  :link '(url-link :tag "Github page" "https://github.com/Bad-ptr/persp-mode.el"))

(defcustom persp-nil-name "none"
  "Name for the nil perspective."
  :group 'persp-mode
  :type 'string
  :set #'(lambda (sym val)
           (when val
             (when persp-mode
               (destructuring-bind (frames . windows)
                   (persp-frames-and-windows-with-persp (persp-get-by-name persp-nil-name))
                 (dolist (win windows)
                   (when (string= persp-nil-name (get-window-persp* win))
                     (set-window-persp* win val))))
               (run-hook-with-args 'persp-renamed-functions nil persp-nil-name val))
             (custom-set-default sym val))))

(defface persp-face-lighter-buffer-not-in-persp
  '((default . (:background "#F00" :foreground "#00F" :weight bold)))
  "Face for the ligher when the current buffer is not in a perspective."
  :group 'persp-mode)
(defface persp-face-lighter-nil-persp
  '((t :inherit bold-italic))
  "Face for the lighter when the current perspective is nil."
  :group 'persp-mode)
(defface persp-face-lighter-default
  '((t :inherit italic))
  "Default face for the lighter.")

(defcustom persp-lighter
  '(:eval (format (propertize " #%.5s"
                              'face (let ((persp (get-current-persp)))
                                      (if persp
                                          (if (persp-contain-buffer-p (current-buffer) persp)
                                              'persp-face-lighter-default
                                            'persp-face-lighter-buffer-not-in-persp)
                                        'persp-face-lighter-nil-persp)))
                  (safe-persp-name (get-current-persp))))
  "Defines how the persp-mode show itself in the modeline."
  :group 'persp-mode
  :type 'sexp)

(defcustom persp-save-dir (expand-file-name "persp-confs/" user-emacs-directory)
  "The directory to/from where perspectives saved/loaded by default.
Autosave files are saved and loaded to/from this directory."
  :group 'persp-mode
  :type 'directory)

(defcustom persp-auto-save-fname "persp-auto-save"
  "Name of the file for auto save/load perspectives on the persp-mode
deactivation or the emacs shutdown."
  :group 'persp-mode
  :type 'string)

(defcustom persp-auto-save-persps-to-their-file t
  "If t -- then a perspective will be autosaved to a file specified
in the `persp-file' perspective parameter."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-auto-save-persps-to-their-file-before-kill nil
  "Whether or not perspectives will be saved before killed."
  :group 'persp-mode
  :type '(choice
          (const :tag "Save perspectives which have `persp-file' parameter"
                 :value persp-file)
          (const :tag "Save all perspectives" :value t)
          (const :tag "Don't save just kill" :value nil)))

(defcustom persp-auto-save-opt 2
  "This variable controls the autosave functionality of the persp-mode:
0 -- do not auto save;
1 -- save on the emacs shutdown and only if the persp-mode active;
2 -- save on the persp-mode deactivation or the emacs shutdown."
  :group 'persp-mode
  :type '(choice
          (const :tag "Do not save"  :value 0)
          (const :tag "Save on exit" :value 1)
          (const :tag "Save on exit and persp-mode deactivation" :value 2)))

(defcustom persp-auto-save-num-of-backups 3
  "How many autosave file backups to keep."
  :group 'persp-mode
  :type 'integer)

(defcustom persp-auto-resume-time 3.0
  "Delay time in seconds before loading from the autosave file. If <= 0 -- do not autoresume."
  :group 'persp-mode
  :type 'float)

(defcustom persp-set-last-persp-for-new-frames t
  "If nil new frames will be created with the 'nil' perspective,
otherwise with a last activated perspective."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-reset-windows-on-nil-window-conf t
  "t -- When a perspective without a window configuration is activated
then delete all windows and show the *scratch* buffer;
function -- run that function;
nil -- do nothing."
  :group 'persp-mode
  :type '(choice
          (const    :tag "Delete all windows" :value t)
          (const    :tag "Do nothing"         :value nil)
          (function :tag "Run function"       :value (lambda () nil))))


(define-widget 'persp-buffer-list-restriction-choices 'lazy
  "Variants of how the buffer-list can be restricted."
  :offset 4
  :tag "\nControl the persp-buffer-list-restricted behaviour"
  :type '(choice
          (const :tag "List all buffers" :value -1)
          (const :tag "List current perspective buffers" :value 0)
          (const :tag "List buffers that aren't in the perspective" :value 1)
          (const :tag "List buffers which unique to the perspective" :value 2)
          (const :tag "List unique buffers, but show all for the nil perspective" :value 2.5)
          (const :tag "List free buffers" :value 3)
          (const :tag "List free buffers, but show all for the nil perspective" :value 3.5)))

(defcustom *persp-restrict-buffers-to* 0
  "Controls the behaviour of the `persp-buffer-list-restricted' function."
  :group 'persp-mode
  :type '(choice
          persp-buffer-list-restriction-choices
          (function :tag "\nRun function with frame as an argument"
                    :value (lambda (f) (buffer-list f)))))

(defcustom persp-restrict-buffers-to-if-foreign-buffer nil
  "Override the *persp-restrict-buffers-to* if the current buffer is not in the
current perspective. If nil -- do not override."
  :group 'persp-mode
  :type '(choice
          (const :tag "Do not override" :value nil)
          persp-buffer-list-restriction-choices
          (function :tag "\nRun function with frame as an argument"
                    :value (lambda (f) (buffer-list f)))))

(defcustom persp-set-frame-buffer-predicate 'restricted-buffer-list
  "t -- set the frame's buffer-predicate parameter to a function returning `t'
    for buffers in current persp;
nil -- do not set the buffer-predicate;
restricted-buffer-list -- return t for buffers contained in the list returned
  from the persp-buffer-list-restricted called without arguments;
number -- the same meaning as for the `*persp-restrict-buffers-to*';
function -- use that function as buffer-predicate."
  :group 'persp-mode
  :type '(choice
          (const :tag "\nConstrain to current perspective's buffers."
                 :value t)
          (const :tag "\nDo not set frames' buffer-predicate parameter."
                 :value nil)
          (const :tag "\nConstrain with persp-buffer-list-restricted."
                 :value restricted-buffer-list)
          persp-buffer-list-restriction-choices
          (function :tag "\nConstrain with a function which take buffer as an argument."
                    :value (lambda (b) b)))
  :set #'(lambda (sym val)
           (custom-set-default sym val)
           (if val
               (if persp-mode
                   (persp-update-frames-buffer-predicate)
                 (if (and (not (daemonp)) (null (cdr (frame-list))))
                     (lexical-let (th)
                       (setq th #'(lambda ()
                                    (run-at-time
                                     10 nil #'(lambda ()
                                                (remove-hook 'emacs-startup-hook th)
                                                (persp-update-frames-buffer-predicate)))))
                       (add-hook 'emacs-startup-hook th))
                   (add-hook 'persp-mode-hook #'persp-update-frames-buffer-predicate)))
             (persp-update-frames-buffer-predicate t))))

;; TODO: remove this var
(defcustom persp-hook-up-emacs-buffer-completion nil
  "If t -- try to restrict read-buffer function of the current completion system."
  :group 'persp-mode
  :type 'boolean)
(make-obsolete-variable
 'persp-hook-up-emacs-buffer-completion
 "`persp-set-read-buffer-function', `persp-set-ido-hooks', `persp-interactive-completion-function'"
 "persp-mode 2.6")

(defsubst persp-set-read-buffer-function (&optional opt)
  (if opt
      (when (not (eq read-buffer-function #'persp-read-buffer))
        (setq persp-saved-read-buffer-function read-buffer-function)
        (setq read-buffer-function #'persp-read-buffer))
    (when (eq read-buffer-function #'persp-read-buffer)
      (setq read-buffer-function persp-saved-read-buffer-function))))
(defcustom persp-set-read-buffer-function nil
  "If t -- set the read-buffer-function to persp-read-buffer."
  :group 'persp-mode
  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)
           (when persp-mode
             (persp-set-read-buffer-function val))))

(defsubst persp-set-ido-hooks (&optional opt)
  (if opt
      (progn
        (add-hook 'ido-make-buffer-list-hook #'persp-restrict-ido-buffers)
        (add-hook 'ido-setup-hook            #'persp-ido-setup))
    (remove-hook 'ido-make-buffer-list-hook #'persp-restrict-ido-buffers)
    (remove-hook 'ido-setup-hook            #'persp-ido-setup)))
(defcustom persp-set-ido-hooks nil
  "If t -- set the ido hooks for buffer list restriction."
  :group 'persp-mode
  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)
           (when persp-mode
             (persp-set-ido-hooks val))))

;; TODO: remove this var, just call the completing-read
(defvar persp-interactive-completion-function #'completing-read
  "The function which is used by the persp-mode
to interactivly read user input with completion.")
(make-obsolete-variable
 'persp-interactive-completion-function
 "`completing-read-function'" "persp-mode 2.7")

(defun persp-update-completion-system (&optional system remove)
  (interactive "i")
  (when (and (not system) (not remove))
    (setq
     system
     (intern
      (funcall persp-interactive-completion-function
               "Set the completion system for persp-mode: "
               '("ido" "completing-read")
               nil t))))
  (if remove
      (progn
        (when (boundp 'persp-interactive-completion-system)
          (when persp-hook-up-emacs-buffer-completion
            (case persp-interactive-completion-system
              (ido (persp-set-ido-hooks))
              (t nil))))
        (setq persp-interactive-completion-function #'completing-read)
        (custom-set-default 'persp-interactive-completion-system 'completing-read))
    (persp-update-completion-system nil t)
    (when system
      (custom-set-default 'persp-interactive-completion-system system)
      (when persp-hook-up-emacs-buffer-completion
        (case persp-interactive-completion-system
          (ido
           (persp-set-ido-hooks t)
           (setq persp-interactive-completion-function #'ido-completing-read))
          (t nil))
        (persp-set-toggle-read-buffer-filter-keys persp-toggle-read-buffer-filter-keys)))))

;; TODO: remove this var
(defcustom persp-interactive-completion-system 'completing-read
  "What completion system to use."
  :group 'persp-mode
  :type '(choice
          (const :tag "ido"             :value ido)
          (const :tag "completing-read" :value completing-read))
  :set #'(lambda (sym val)
           (if persp-mode
               (persp-update-completion-system val)
             (custom-set-default sym val))))
(make-obsolete-variable
 'persp-interactive-completion-system
 "`persp-set-read-buffer-function', `persp-set-ido-hooks', `persp-interactive-completion-function'"
 "persp-mode 2.6")

(define-widget 'persp-init-frame-behaviour-choices 'lazy
  "Choices of the init-frame behavoiurs for the persp-mode."
  :offset 4
  :tag "\nControl how frames initialized by persp-mode"
  :type '(choice
          (const :tag "Restore window-configuration" :value t)
          (const :tag "Do not restore window-configuration" :value nil)
          (const :tag "Set persp-ignore-wconf flag for frame" :value persp-ignore-wconf)
          (const :tag "Set persp-ignore-wconf-once flag for frame" :value persp-ignore-wconf-once)
          (const :tag "Create a new random auto-perspective for the new frame" :value auto-temp)
          (const :tag "Create a new perspective for the new frame and prompt for it's name" :value prompt)
          (string :tag "Use/create the perspective with a name" :value "pfnf")
          (function :tag "Run this function" :value (lambda (frame &optional new-frame-p) nil))))

(defcustom persp-init-frame-behaviour t
  "Control the behaviour of how frames initialized."
  :group 'persp-mode
  :type 'persp-init-frame-behaviour-choices)

(defcustom persp-init-new-frame-behaviour-override -1
  "Override the `persp-init-frame-behaviour` for new frames."
  :group 'persp-mode
  :type '(choice
          (const :tag "Do not override" : value -1)
          persp-init-frame-behaviour-choices))

(defcustom persp-interactive-init-frame-behaviour-override -1
  "Override the `persp-init-frame-behaviour' when the `make-frame' was called interactively."
  :group 'persp-mode
  :type '(choice
          (const :tag "Do not override" :value -1)
          persp-init-frame-behaviour-choices))

(defcustom persp-emacsclient-init-frame-behaviour-override -1
  "Override the `persp-init-frame-behaviour' variable for frames created using the
emacsclient -[c|t]."
  :group 'persp-mode
  :type '(choice
          (const :tag "Do not override" :value -1)
          persp-init-frame-behaviour-choices))

(defcustom persp-server-switch-behaviour 'only-file-windows-for-client-frame
  "Controls the behaviour of the server-switch-hook."
  :group 'persp-mode
  :type '(choice
          (const :tag "Do nothing" :value nil)
          (const :tag "Leave only windows displaing files for edit\n\
(files that was supplied as parameters to emacsclient)" :value only-file-windows)
          (const :tag "For the new frame(created by emacsclient -c ...)\n\
leave only windows displaing files for edit" :value only-file-windows-for-client-frame)
          (function :tag "Run this function" :value (lambda (frame buflist) nil)))
  :set #'(lambda (sym val)
           (custom-set-default sym val)
           (if persp-mode
             (persp-update-frame-server-switch-hook)
            (add-hook 'persp-mode-hook #'persp-update-frame-server-switch-hook))))

;; TODO: remove this var
(defcustom persp-ignore-wconf-of-frames-created-to-edit-file t
  "If t -- set the persp-ignore-wconf frame parameter
to t for frames that were created by emacsclient with file arguments.
Also delete windows not showing that files
(this is because server-switch-hook runs after after-make-frames);
If function -- run that function."
  :group 'persp-mode
  :type '(choice
          (const    :tag "Ignore window configuration" :value t)
          (const    :tag "Do as usual"  :value nil)
          (function :tag "Run function" :value (lambda () nil))))
(make-obsolete-variable
 'persp-ignore-wconf-of-frames-created-to-edit-file
 "`persp-emacsclient-frame-to-edit-file-behavoiur'" "persp-mode 2.0")

(defcustom persp-add-buffer-on-find-file t
  "If t -- add a buffer with opened file to current perspective."
  :group 'persp-mode
  :type '(choice
          (const :tag "Always add" :value t)
          (const :tag "Newer add" :value nil)
          (const :tag "\nAdd if not matching any predicate from `persp-auto-persp-alist'"
                 :value if-not-autopersp)
          (const :tag "\nAlways add but do not switch if the buffer matches any predicate \
from `persp-auto-persp-alist'"
                 :value add-but-not-switch-if-autopersp)))


(defcustom persp-add-buffer-on-after-change-major-mode nil
  "t -- add the current buffer to the current perspective when
the `after-change-major-mode-hook' fires;
nil -- do not add;
'free -- add only _free_ buffers;
function -- run that function."
  :group 'persp-mode
  :type '(choice
          (const :tag "Always add" :value t)
          (const :tag "Don't add" :value nil)
          (const :tag "\nAdd if the buffer is not already in any other persp"
                 :value free)
          (function :tag "Run this function" :value (lambda () nil)))
  :set #'(lambda (sym val)
           (custom-set-default sym val)
           (when persp-mode
             (if val
                 (add-hook 'after-change-major-mode-hook #'persp-after-change-major-mode-h t)
               (remove-hook 'after-change-major-mode-hook #'persp-after-change-major-mode-h)))))

(defcustom persp-switch-to-added-buffer t
  "If t then after you add a buffer to the current perspective
the currently selected window will be switched to that buffer."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-when-kill-switch-to-buffer-in-perspective nil
  "If t -- then after a buffer is killed the current window
will be switched to some previous buffer in the current perspective,
otherwise let  the emacs decide what to do."
  :group 'persp-mode
  :type 'boolean)

(define-widget 'persp-kill-foreign-buffer-behaviour-choices 'lazy
  "What to do when manually killing a buffer that is not in the current perspective."
  :offset 4
  :tag "\nControl the persp-kill-buffer-query-function behaviour."
  :type '(choice
          (const    :tag "Ask what to do" :value ask)
          (const    :tag "\nDon't ask if a buffer belongs only to weak perspectives"
                    :value dont-ask-weak)
          (const    :tag "Just kill"      :value kill)
          (function :tag "Run function" :value (lambda () t))
          (const    :tag "\nDo not suggest foreign buffer to the user(kill buffer)"
                    :value nil)))

(defcustom persp-kill-foreign-buffer-behaviour 'dont-ask-weak
  "What to do when manually killing a buffer that is not in the current perspective."
  :group 'persp-mode
  :type 'persp-kill-foreign-buffer-behaviour-choices)
(define-obsolete-variable-alias 'persp-kill-foreign-buffer-action
  'persp-kill-foreign-buffer-behaviour "persp-mode 2.9.6")

(defcustom persp-kill-foreign-indirect-buffer-behaviour-override 'do-not-override
  "What to do if killing an indirect buffer."
  :group 'persp-mode
  :type '(choice
          persp-kill-foreign-buffer-behaviour-choices
          (const :tag "Do the action as if it were applyed to base buffer."
                 :value as-base-buffer)
          (const :tag "Do not override" :value do-not-override)))

(defcustom persp-autokill-buffer-on-remove nil
  "Kill the buffer if it removed from every(or non weak) perspective."
  :group 'persp-mode
  :type '(choice
          (const :tag "Just kill" :value kill) ;; or t
          (const :tag "Kill if buffer belongs only to weak perspectives" :value kill-weak)
          (const :tag "Do not kill" :value nil)))

(defcustom persp-autokill-persp-when-removed-last-buffer 'hide-auto
  "Kill the perspective if no buffers left in it."
  :group 'persp-mode
  :type '(choice
          (const :tag "Just kill" :value kill) ;; or t
          (const :tag "Kill auto perspectives" :value kill-auto)
          (const :tag "Hide" :value hide)
          (const :tag "Hide auto perspectives" :value hide-auto)
          (const :tag "Do not kill" :value nil)
          (function :tag "\nRun this function with persp as an argument"
                    :value (lambda (p) p))))

(defcustom persp-common-buffer-filter-functions
  (list #'(lambda (b) (or (string-prefix-p " " (buffer-name b))
                     (eq (buffer-local-value 'major-mode b) 'helm-major-mode))))
  "Common buffer filters.
The list of functions wich takes a buffer as an argument.
If one of these functions returns a non nil value the buffer considered as 'filtered out'."
  :group 'persp-mode
  :type 'hook
  :set #'(lambda (sym val)
           (custom-set-default sym (mapcar #'byte-compile val))))

(defcustom persp-buffer-list-restricted-filter-functions nil
  "Additional filters for use inside pthe `persp-buffer-list-restricted'."
  :group 'persp-mode
  :type 'hook
  :set #'(lambda (sym val)
           (custom-set-default sym (mapcar #'byte-compile val))))

(defcustom persp-add-buffer-on-after-change-major-mode-filter-functions nil
  "Additional filters to know which buffers we dont want to add to the current perspective
after the `after-change-major-mode-hook' is fired."
  :group 'persp-mode
  :type 'hook
  :set #'(lambda (sym val)
           (custom-set-default sym (mapcar #'byte-compile val))))

(defcustom persp-filter-save-buffers-functions
  (list #'(lambda (b) (string-prefix-p "*" (buffer-name b))))
  "Additional filters to not save unneeded buffers."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-save-buffer-functions
  (list #'(lambda (b)
            (when (persp-buffer-filtered-out-p
                   b persp-filter-save-buffers-functions)
              'skip))
        #'persp-tramp-save-buffer
        #'(lambda (b)
            (when (eq 'dired-mode (buffer-local-value 'major-mode b))
              `(def-buffer ,(buffer-name b)
                 ,(buffer-local-value 'default-directory b)
                 ,(buffer-local-value 'major-mode b))))
        #'(lambda (b)
            `(def-buffer ,(buffer-name b)
               ,(buffer-file-name b)
               ,(buffer-local-value 'major-mode b))))
  "Convert a buffer to a structure that could be saved to a file.
If a function return nil -- follow to the next function in the list.
If a function return 'skip -- don't save a buffer."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-load-buffer-functions
  (list #'persp-buffer-from-savelist)
  "Restore a buffer from a saved structure.
If a function return nil -- follow to the next function in the list.
If a function return 'skip -- don't restore a buffer."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-mode-hook nil
  "The hook that's run after the `persp-mode' has been activated."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-mode-deactivated-hook nil
  "Runs when the persp-mode is deactivated."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-created-functions nil
  "The list of functions that runs after a perspective has been created.
It must accept two argument -- the created perspective and the hash to which this perspective
will be placed, you could be interested if that hash is the `*persp-hash*' or some other."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-renamed-functions nil
  "Hooks to runs if a perspective was renamed.
They are called with three arguments:
1) perspective; 2) old name; 3) new name."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-before-kill-functions nil
  "The list of functions that runs just before a perspective will be destroyed.
It's single argument is the perspective that will be killed."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-before-switch-functions nil
  "The list of functions that runs before actually switching to a perspective.
These functions must take two arguments -- a name of a perspective to switch
(it could be a name of an nonexistent perspective or it could be the same as current)
and a frame or a window for which the switching takes place."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-activated-functions nil
  "Functions that runs after a perspective has been activated.
These functions must take one argument -- a symbol,
if it is eq 'frame -- then the perspective is activated for the current frame,
if it is eq 'window -- then the perspective is activated for the current window.
The activated perspective is available with (get-current-persp)."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-before-deactivate-functions nil
  "Functions that runs before the current perspective has been deactivated for selected frame or window.
These functions must take one argument -- a symbol,
if it is eq 'frame -- then the perspective will be deactivated for the current frame,
if it is eq 'window -- then the perspective will be deactivated for the current window.
The perspective is available with (get-current-persp)."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-before-save-state-to-file-functions nil
  "Functions to run before saving perspectives to a file.
Each function in this list will be called with 3 arguments:
1) a file name to which perspectives will be saved;
2) a hash with perspectives;
3) a boolean argument indicating if the persp-file parameter of perspectives must be set."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-after-load-state-functions
  (list #'(lambda (file phash persp-names)
            (when (eq phash *persp-hash*)
              (persp-update-frames-window-confs persp-names))))
  "Functions that run after perspectives state was loaded.
These functions must take 3 arguments:
1) a file from which the state was loaded;
2) a hash in which loaded perspectives were placed;
3) names(list) of perspectives that was loaded."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-use-workgroups (and (version< emacs-version "24.4")
                                     (locate-library "workgroups.el"))
  "If t -- use the workgroups.el package for saving/restoring windows configurations."
  :group 'persp-mode
  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)
           ;; require workgroups if we are going to use it
           (when persp-use-workgroups
             ;;(require 'workgroups)
             (unless (fboundp 'wg-make-wconfig)
               (autoload 'wg-make-wconfig "workgroups"
                 "Return a new Workgroups window config from `selected-frame'." ))
             (unless (fboundp 'wg-restore-wconfig)
               (autoload 'wg-restore-wconfig "workgroups"
                 "Restore WCONFIG in `selected-frame'." )))))

(defcustom persp-restore-window-conf-method t
  "Defines how to restore window configurations for the new frames:
t -- the standard action.
function -- run that function."
  :group 'persp-mode
  :type '(choice (const :tag "Standard action" :value t)
                 (function :tag "Run function" :value (lambda (frame persp new-frame-p) nil))))

(defcustom persp-window-state-get-function
  (if persp-use-workgroups
      #'(lambda (&optional frame rwin)
          (when (or frame (setq frame (selected-frame)))
            (with-selected-frame frame (wg-make-wconfig))))
    (if (version< emacs-version "24.4")
        #'(lambda (&optional frame rwin)
            (when (or rwin (setq rwin (frame-root-window (or frame (selected-frame)))))
              (when (fboundp 'window-state-get)
                (window-state-get rwin))))
      #'(lambda (&optional frame rwin)
          (when (or rwin (setq rwin (frame-root-window (or frame (selected-frame)))))
            (window-state-get rwin t)))))
  "The function for getting the window configuration of a frame, accept two optional parameters:
first -- a frame(default is the selected one)
second -- a root window(default is the root window of the selected frame)."
  :group 'persp-mode
  :type 'function)

(defcustom persp-window-state-put-function
  (if persp-use-workgroups
      #'(lambda (pwc &optional frame rwin)
          (when (or frame (setq frame (selected-frame)))
            (with-selected-frame frame
              (flet ((wg-switch-to-window-buffer
                      (win)
                      "Switch to a buffer determined from WIN's fname and bname.\n\
Return the buffer if it was found, nil otherwise."
                      (wg-abind win (fname bname)
                                (cond ((wg-awhen (get-buffer bname) (persp-switch-to-buffer it)))
                                      (t (persp-switch-to-buffer wg-default-buffer) nil)))))
                (wg-restore-wconfig pwc)))))
    #'(lambda (pwc &optional frame rwin)
        (when (or rwin (setq rwin (frame-root-window (or frame (selected-frame)))))
          (when (fboundp 'window-state-put)
            (window-state-put pwc rwin t)))))
  "The function for restoring a window configuration. Accept a window configuration
obtained by the `persp-window-state-get-function' and two optional arguments:
one -- a frame(default is the selected frame)
and another -- root window(default is the root window of the selected frame)."
  :group 'persp-mode
  :type 'function)

(defcustom persp-buffer-list-function (symbol-function 'buffer-list)
  "The function that is used mostly internally by persp-mode functions
to get a list of all buffers."
  :group 'persp-mode
  :type 'function)

(defcustom persp-dont-count-weaks-in-restricted-buffer-list nil
  "if t -- dont count weak perspectives in `persp-buffer-list-restricted'.
For now it makes any effect only if the value of the `*persp-restrict-buffers-to*' and friends
is 2, 2.5, 3 or 3.5."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-auto-persp-alist nil
  "Alist of auto-persp definitions."
  :group 'persp-mode
  :tag "Auto perspectives"
  :type '(alist :key-type (string :tag "Name")
                :value-type (alist :tag "Parameters"
                                   :key-type (symbol :tag "Keyword"))))


;; Global variables:

;; check if the initial-buffer-choice may be a function (emacs >= 24.4)
(defvar persp-is-ibc-as-f-supported
  (or
   (not (version< emacs-version "24.4"))
   (not
    (null
     (assq 'function
           (cdr (getf (symbol-plist 'initial-buffer-choice) 'custom-type))))))
  "t if the `initial-buffer-choice' as a function is supported in your emacs,
otherwise nil.")

(defvar persp-minor-mode-menu nil
  "Menu for the persp-mode.")

(defvar *persp-hash* nil
  "The hash table that contain perspectives")

(defvar persp-temporarily-display-buffer nil
  "This variable dynamically bound to t inside the `persp-temporarily-display-buffer'")

(defvar persp-saved-read-buffer-function read-buffer-function
  "Save the `read-buffer-function' to restore it on deactivation.")

(defvar persp-last-persp-name persp-nil-name
  "The last activated perspective. A new frame will be created with that perspective
if `persp-set-last-persp-for-new-frames' is t.")

(defvar persp-special-last-buffer nil
  "The special variable to handle the case when new frames switches the selected window buffer
to a wrong one.")

(defvar persp-frame-buffer-predicate nil
  "Current buffer-predicate.")

(defvar persp-frame-buffer-predicate-buffer-list-cache nil
  "Variable to cache the perspective buffer list for buffer-predicate.")

(defvar persp-frame-server-switch-hook nil
  "Current persp-server-switch-hook.")

(defvar persp-disable-buffer-restriction-once nil
  "The flag used for toggling buffer filtering during read-buffer.")

(defvar persp-inhibit-switch-for nil
  "List of frames and windows for which the switching of perspectives is inhibited.")

(defvar persp-read-multiple-exit-minibuffer-function #'exit-minibuffer
  "Function to call to exit minibuffer when reading multiple candidates.")

(defvar persp-buffer-props-hash (when persp-mode
                                  (make-hash-table :test #'eq :size 10))
  "Cache to store buffer properties.")


(defvar persp-backtrace-frame-function
  (if (version< emacs-version "24.4")
      #'(lambda (nframes &optional base)
          (let ((i (if base
                       (let ((k 8) found bt)
                         (while (and (not found)
                                     (setq bt (cadr (funcall #'backtrace-frame
                                                             (incf k)))))
                           ;; (message "%s:%s" k (backtrace-frame k))
                           (when (eq bt base) (setq found t)))
                         (when found (+ nframes (- k 3))))
                     (+ nframes 6))))
            (when i
              (funcall #'backtrace-frame i))))
    #'backtrace-frame)
  "Backtrace function with base argument.")


(defcustom persp-switch-wrap t
  "Whether `persp-next' and `persp-prev' should wrap."
  :group 'persp-mode
  :type 'boolean)


;; Key bindings:

(define-prefix-command 'persp-key-map)

(defvar persp-mode-map (make-sparse-keymap)
  "The keymap with a prefix for the persp-mode.")

(define-key persp-key-map (kbd "n") #'persp-next)
(define-key persp-key-map (kbd "p") #'persp-prev)
(define-key persp-key-map (kbd "s") #'persp-frame-switch)
(define-key persp-key-map (kbd "S") #'persp-window-switch)
(define-key persp-key-map (kbd "r") #'persp-rename)
(define-key persp-key-map (kbd "c") #'persp-copy)
(define-key persp-key-map (kbd "C") #'persp-kill)
(define-key persp-key-map (kbd "z") #'persp-save-and-kill)
(define-key persp-key-map (kbd "a") #'persp-add-buffer)
(define-key persp-key-map (kbd "b") #'persp-switch-to-buffer)
(define-key persp-key-map (kbd "t") #'persp-temporarily-display-buffer)
(define-key persp-key-map (kbd "i") #'persp-import-buffers)
(define-key persp-key-map (kbd "I") #'persp-import-win-conf)
(define-key persp-key-map (kbd "k") #'persp-remove-buffer)
(define-key persp-key-map (kbd "K") #'persp-kill-buffer)
(define-key persp-key-map (kbd "w") #'persp-save-state-to-file)
(define-key persp-key-map (kbd "W") #'persp-save-to-file-by-names)
(define-key persp-key-map (kbd "l") #'persp-load-state-from-file)
(define-key persp-key-map (kbd "L") #'persp-load-from-file-by-names)
(define-key persp-key-map (kbd "o") #'(lambda ()
                                        (interactive)
                                        (persp-mode -1)))


(defun persp-set-keymap-prefix (prefix)
  (interactive
   (list
    (read-key-sequence
     "Now press a key sequence to be used as the persp-key-map prefix: ")))
  (when prefix
    (when (boundp 'persp-keymap-prefix)
      (substitute-key-definition 'persp-key-map nil persp-mode-map))
    (define-key persp-mode-map prefix 'persp-key-map)
    (custom-set-default 'persp-keymap-prefix prefix)))

(defcustom persp-keymap-prefix (kbd "C-c p")
  "The prefix for activating the persp-mode keymap."
  :group 'persp-mode
  :type 'key-sequence
  :set #'(lambda (sym val) (persp-set-keymap-prefix val)))

;; TODO: remove this function
(defun persp-set-toggle-read-buffer-filter-keys (keys)
  (interactive
   (list
    (read-key-sequence
     "Now press a key sequence to be used for toggling persp filters during the read-buffer: ")))
  (setcdr (assq 'toggle-persp-buffer-filter persp-read-multiple-keys) keys)
  (custom-set-default 'persp-toggle-read-buffer-filter-keys keys))
(define-obsolete-function-alias
  'persp-set-toggle-read-persp-filter-keys 'persp-set-toggle-read-buffer-filter-keys
  "persp-mode 2.9")

(defcustom persp-read-multiple-keys `((toggle-persp-buffer-filter . ,(kbd "C-x C-p"))
                                      (push-item . ,(kbd "C-<return>"))
                                      (pop-item  . ,(kbd "M-<return>")))
  "Keybindings to use while prompting for multiple items."
  :group 'persp-mode
  :tag "Keys for reading multiple items"
  :type '(alist :key-type symbol
                :value-type key-sequence))

(defcustom persp-toggle-read-buffer-filter-keys (kbd "C-x C-p")
  "Keysequence to toggle the buffer filtering during read-buffer."
  :group 'persp-mode
  :type 'key-sequence
  :set #'(lambda (sym val)
           (persp-set-toggle-read-buffer-filter-keys val)))
(define-obsolete-variable-alias
  'persp-toggle-read-persp-filter-keys 'persp-toggle-read-buffer-filter-keys
  "persp-mode 2.9")


;; Perspective struct:

(defstruct (perspective
            (:conc-name persp-)
            (:constructor make-persp))
  (name "")
  (buffers nil)
  (window-conf nil)
  ;; reserved parameters: dont-save-to-file, persp-file.
  (parameters nil)
  (weak nil)
  (auto nil)
  (hidden nil))

(defvar persp-nil-wconf nil
  "Window configuration for the `nil' perspective.")

(defvar persp-nil-parameters nil
  "Parameters of the `nil' perspective.")

(defvar persp-nil-hidden nil
  "Hidden filed for the `nil' perspective.")

(defun persp-buffer-list (&optional frame window)
  (safe-persp-buffers (get-current-persp frame window)))

(defun* persp-buffer-list-restricted
    (&optional (frame (selected-frame))
               (option *persp-restrict-buffers-to*)
               (option-foreign-override persp-restrict-buffers-to-if-foreign-buffer)
               sure-not-killing)
  (unless frame (setq frame (selected-frame)))
  (unless option (setq option 0))
  (let* ((cpersp (get-current-persp frame))
         (curbuf (current-buffer))
         (cb-foreign (not (persp-contain-buffer-p curbuf cpersp))))
    (when (and option-foreign-override cb-foreign)
      (setq option option-foreign-override))
    (if (functionp option)
        (funcall option frame)
      (when (= option 2.5)
        (setq option (if (null cpersp) -1 2)))
      (when (= option 3.5)
        (setq option (if (null cpersp) -1 3)))
      (let ((bl
             (case option
               (-1 (funcall persp-buffer-list-function frame))
               (0 (if cpersp
                      (append (persp-buffers cpersp) nil)
                    (funcall persp-buffer-list-function frame)))
               (1 (let ((ret (if cpersp
                                 (let ((pbs (persp-buffers cpersp)))
                                   (delete-if #'(lambda (b) (memq b pbs))
                                              (funcall persp-buffer-list-function frame)))
                               nil)))
                    (unless (persp-contain-buffer-p curbuf cpersp)
                      (setq ret (cons curbuf (delete curbuf ret))))
                    ret))
               (2 (let ((ret (delete-if #'(lambda (b)
                                            (persp-buffer-in-other-p*
                                             b cpersp persp-dont-count-weaks-in-restricted-buffer-list))
                                        (if cpersp
                                            (append (persp-buffers cpersp) nil)
                                          (funcall persp-buffer-list-function frame)))))
                    ret))
               (3 (let ((ret (delete-if #'(lambda (b)
                                            (or
                                             (and cpersp
                                                  (persp-contain-buffer-p b cpersp))
                                             (persp-buffer-in-other-p*
                                              b cpersp persp-dont-count-weaks-in-restricted-buffer-list)))
                                        (funcall persp-buffer-list-function frame))))
                    ret)))))
        (when persp-buffer-list-restricted-filter-functions
          (setq bl (delete-if #'(lambda (b)
                                  (persp-buffer-filtered-out-p
                                   b persp-buffer-list-restricted-filter-functions))
                              bl)))
        (when (and
               (not sure-not-killing) cpersp
               (symbolp this-command)
               persp-kill-foreign-buffer-behaviour
               (string-match-p "^.*?kill-buffer.*?$" (symbol-name this-command))
               (not (memq curbuf bl))
               (not (persp-buffer-filtered-out-p curbuf)))
          (setq bl (cons curbuf bl)))
        bl))))

(defmacro* with-persp-buffer-list
    ((&key (buffer-list-function persp-buffer-list-function)
           (restriction *persp-restrict-buffers-to*)
           (restriction-foreign-override persp-restrict-buffers-to-if-foreign-buffer)
           sortp cache)
     &rest body)
  (let ((pblf-body `(persp-buffer-list-restricted frame)))
    (when sortp (setq pblf-body `(sort ,pblf-body (with-no-warnings ',sortp))))
    `(let ((*persp-restrict-buffers-to* ,restriction)
           (persp-restrict-buffers-to-if-foreign-buffer ,restriction-foreign-override)
           ,@(if cache `(persp-buffer-list-cache) nil))
       (flet ((buffer-list (&optional frame)
                           ,(if cache
                                `(if persp-buffer-list-cache
                                     persp-buffer-list-cache
                                   (setq persp-buffer-list-cache ,pblf-body))
                              pblf-body)))
         ,@body))))

(defmacro with-persp-ido-hooks (&rest body)
  `(let ((ido-make-buffer-list-hook ido-make-buffer-list-hook)
         (ido-setup-hook ido-setup-hook))
     (persp-set-ido-hooks t)
     ,@body))

(defun safe-persp-name (p)
  (if p (persp-name p)
    persp-nil-name))

(defun safe-persp-buffers (p)
  (if p (persp-buffers p)
    (funcall persp-buffer-list-function)))

(defun safe-persp-window-conf (p)
  (if p (persp-window-conf p)
    persp-nil-wconf))

(defun safe-persp-parameters (p)
  (if p (persp-parameters p)
    persp-nil-parameters))

(defun safe-persp-weak (p)
  (if p (persp-weak p)
    t))

(defun safe-persp-auto (p)
  (if p (persp-auto p)
    nil))

(defun safe-persp-hidden (p)
  (if p (persp-hidden p)
    persp-nil-hidden))

(defun* modify-persp-parameters (alist &optional (persp (get-current-persp)))
  (loop for (name . value) in alist
        do (set-persp-parameter name value persp)))

(defun* set-persp-parameter (param-name &optional value (persp (get-current-persp)))
  (let* ((params (safe-persp-parameters persp))
         (old-cons (assq param-name params)))
    (if old-cons
        (setcdr old-cons value)
      (if persp
          (setf (persp-parameters persp)
                (acons param-name value params))
        (setq persp-nil-parameters
              (acons param-name value params))))))

(defun* persp-parameter (param-name &optional (persp (get-current-persp)))
  (alist-get param-name (safe-persp-parameters persp)))

(defun* delete-persp-parameter (param-name &optional (persp (get-current-persp)))
  (when (and (not (null param-name)) (symbolp param-name))
    (if persp
        (setf (persp-parameters persp)
              (delq (assq param-name (persp-parameters persp))
                    (persp-parameters persp)))
      (setq persp-nil-parameters
            (delq (assq param-name persp-nil-parameters)
                  persp-nil-parameters)))))

(defun persp--buffer-in-persps (buf)
  (cdr
   (assq 'persp-buffer-in-persps
         (gethash (persp-get-buffer-or-null buf)
                  persp-buffer-props-hash))))

(defun persp--buffer-in-persps-set (buf persps)
  (let* ((buf-props (gethash buf persp-buffer-props-hash))
         (cons (assq 'persp-buffer-in-persps buf-props)))
    (if cons
        (setf (cdr cons) persps)
      (setq cons (cons 'persp-buffer-in-persps persps))
      (push cons buf-props)
      (puthash buf buf-props persp-buffer-props-hash))))

(defun persp--buffer-in-persps-add (buf persp)
  (persp--buffer-in-persps-set
   buf (cons persp (persp--buffer-in-persps buf))))

(defun persp--buffer-in-persps-remove (buf persp)
  (persp--buffer-in-persps-set
   buf (delq persp (persp--buffer-in-persps buf))))


;; Used in mode defenition:

(defun persp-mode-start-and-remove-from-make-frame-hook (f)
  (persp-mode 1)
  (remove-hook 'after-make-frame-functions #'persp-mode-start-and-remove-from-make-frame-hook))

(defun persp-asave-on-exit (&optional interactive-query)
  (when persp-mode
    (if (> persp-auto-save-opt 0)
        (condition-case-unless-debug err
            (persp-save-state-to-file)
          (error
           (message "[persp-mode] Error: Can not autosave perspectives -- %s"
                    err)
           (when (or noninteractive
                     (progn
                       (when (null (persp-frame-list-without-daemon))
                         (make-frame))
                       (null (persp-frame-list-without-daemon))))
             (setq interactive-query nil))
           (if interactive-query
               (yes-or-no-p "persp-mode can not save perspectives, do you want to exit anyway?")
             t)))
      t)))
(defun persp-kill-emacs-h ()
  (persp-asave-on-exit nil))

(defun persp-kill-emacs-query-function ()
  (if persp-mode
      (when (persp-asave-on-exit t)
        (remove-hook 'kill-emacs-hook #'persp-kill-emacs-h)
        t)
    t))

(defun persp-special-last-buffer-make-current ()
  (setq persp-special-last-buffer (current-buffer)))


;; Auto persp functions:

(defun persp-auto-persp-parameters (name)
  (cdr (assoc name persp-auto-persp-alist)))
(defun persp--auto-persp-pickup-buffer (a-p-def buffer)
  (let ((action (alist-get :main-action a-p-def)))
    (when (functionp action)
      (funcall action buffer))))
(defun persp-auto-persp-pickup-bufferlist-for (name bufferlist)
  (let ((a-p-def (persp-auto-persp-parameters name)))
    (when a-p-def
      (mapc (apply-partially #'persp--auto-persp-pickup-buffer a-p-def)
            bufferlist))))
(defun persp-auto-persps-pickup-bufferlist (bufferlist)
  (mapc
   #'(lambda (name) (persp-auto-persp-pickup-bufferlist-for name bufferlist))
   (mapcar #'car persp-auto-persp-alist)))
(defun persp-auto-persp-pickup-buffers-for (name)
  (persp-auto-persp-pickup-bufferlist-for name (funcall persp-buffer-list-function)))
(defun persp-auto-persps-pickup-buffers ()
  (interactive)
  (persp-auto-persps-pickup-bufferlist (funcall persp-buffer-list-function)))

(defun persp-buffer-match-auto-persp-p (buffer-or-name)
  (let ((buffer (persp-get-buffer-or-null buffer-or-name))
        pred)
    (car-safe
     (find-if #'(lambda (a-p-def)
                  (and (setq pred (alist-get :generated-predicate a-p-def))
                       (funcall pred buffer)))
              persp-auto-persp-alist
              :key #'cdr))))
(defun persp-auto-persps-for-buffer (buffer-or-name)
  (let ((buffer (persp-get-buffer-or-null buffer-or-name)))
    (remove-if #'(lambda (pred) (funcall pred buffer))
               persp-auto-persp-alist
               :key #'(lambda (a-p-cons)
                        (alist-get :generated-predicate (cdr a-p-cons))))))

(defun persp-auto-persp-activate-hooks (name)
  (let ((hooks
         (alist-get :hooks
                    (persp-auto-persp-parameters name))))
    (mapc #'(lambda (hook-cons)
              (add-hook (car hook-cons) (cdr hook-cons)))
          hooks)))
(defun persp-auto-persp-deactivate-hooks (name)
  (let ((hooks
         (alist-get :hooks
                    (persp-auto-persp-parameters name))))
    (mapc #'(lambda (hook-cons)
              (remove-hook (car hook-cons) (cdr hook-cons)))
          hooks)))
(defun persp-auto-persps-activate-hooks ()
  (mapc #'persp-auto-persp-activate-hooks
        (mapcar #'car persp-auto-persp-alist)))
(defun persp-auto-persps-deactivate-hooks ()
  (mapc #'persp-auto-persp-deactivate-hooks
        (mapcar #'car persp-auto-persp-alist)))

(defsubst persp--generate-predicate-loop-any-all (items-list condition &rest body)
  (if items-list
      (let (all noquote)
        (setq items-list
              (typecase items-list
                (function (list items-list))
                (list (if (persp-regexp-p items-list) (list items-list) items-list))
                (t (list items-list))))
        (setq noquote (eq :noquote (car items-list)))
        (when noquote (setq items-list (cadr items-list)))
        (when (listp items-list)
          (setq all (eq :all (car items-list)))
          (when all (pop items-list))
          (unless noquote (setq items-list `',items-list)))
        (let* ((cnd `(member-if
                      #'(lambda (item)
                          (setq cond-result
                                ,(if all
                                     `(not ,condition)
                                   condition)))
                      ,items-list)))
          `(let (cond-result)
             (when ,(if all `(not ,cnd) cnd)
               ,@body))))
    `(let (cond-result)
       ,@body)))
(defun* persp--generate-buffer-predicate
    (&key buffer-name file-name mode mode-name minor-mode minor-mode-name predicate
          (true-value (if predicate 'cond-result t))
          &allow-other-keys)
  (let ((predicate-body true-value))
    (when predicate
      (setq predicate-body
            (persp--generate-predicate-loop-any-all
             predicate '(apply item buffer rest-args) predicate-body)))
    (when file-name
      (setq predicate-body
            (persp--generate-predicate-loop-any-all
             file-name '(persp-string-match-p item (buffer-file-name buffer))
             predicate-body)))
    (when buffer-name
      (setq predicate-body
            (persp--generate-predicate-loop-any-all
             buffer-name '(persp-string-match-p item (buffer-name buffer))
             predicate-body)))
    (when minor-mode-name
      (setq predicate-body
            (persp--generate-predicate-loop-any-all
             minor-mode-name
             `(let ((regexp item))
                ,(persp--generate-predicate-loop-any-all
                  '(:noquote minor-mode-alist)
                  '(persp-string-match-p regexp (format-mode-line item))
                  t))
             predicate-body)))
    (when minor-mode
      (setq predicate-body
            (persp--generate-predicate-loop-any-all
             minor-mode
             `(cond
               ((symbolp item) (bound-and-true-p item))
               ((persp-regexp-p item) (let ((regexp item))
                                        ,(persp--generate-predicate-loop-any-all
                                          '(:noquote minor-mode-list)
                                          '(and
                                            (bound-and-true-p item)
                                            (persp-string-match-p regexp item))
                                          t)))
               (t nil))
             predicate-body)))

    (when mode-name
      (setq predicate-body
            (persp--generate-predicate-loop-any-all
             mode-name '(persp-string-match-p item (format-mode-line mode-name))
             predicate-body)))
    (when mode
      (setq predicate-body
            (persp--generate-predicate-loop-any-all
             mode '(cond
                    ((symbolp item) (eq item major-mode))
                    ((persp-regexp-p item) (persp-string-match-p item (symbol-name major-mode)))
                    (t nil))
             predicate-body)))
    (eval `(lambda (buffer &rest rest-args)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer ,predicate-body))))))

(defun persp--auto-persp-default-on-match (state)
  (persp-add-buffer (alist-get 'buffer state)
                    (alist-get 'persp state)
                    nil nil)
  state)
(defun persp--auto-persp-default-after-match (state)
  (let ((persp (alist-get 'persp state))
        (noauto (alist-get :noauto state))
        (weak (alist-get :weak state))
        (parameters (alist-get :parameters state)))
    (when persp
      (when (not noauto)
        (setf (persp-auto persp) t))
      (when weak
        (setf (persp-weak persp) t))
      (modify-persp-parameters parameters persp)))
  (let ((persp-name (alist-get 'persp-name state))
        (switch (alist-get :switch state)))
    (persp-unhide persp-name)
    (case switch
      ('nil nil)
      (window (persp-window-switch persp-name))
      (frame (persp-frame-switch persp-name))
      (t (persp-switch persp-name)))
    (when switch
      (persp-switch-to-buffer (alist-get 'buffer state))))
  state)

;;;###autoload
(defun* persp-def-auto-persp
    (name &rest keyargs
          &key buffer-name file-name mode mode-name minor-mode minor-mode-name
          predicate hooks dyn-env get-name get-buffer get-persp
          switch parameters noauto weak user-data
          on-match after-match dont-pick-up-buffers delete)

  (if delete
      (let ((ap-cons (assoc name persp-auto-persp-alist)))
        (persp-auto-persp-deactivate-hooks name)
        (setq persp-auto-persp-alist
              (delq ap-cons persp-auto-persp-alist)))

    (let (auto-persp-parameters
          generated-predicate generated-hook
          hook-body main-action)

      (loop for (key val) on keyargs by #'cddr
            when (and val (not (or (eq key :dont-pick-up-buffers))))
            do (push (cons key (if (and (functionp val)
                                        (not (or (eq key :mode) (eq key :minor-mode)))
                                        (null (byte-code-function-p val)))
                                   val ;;(byte-compile val)
                                 val))
                     auto-persp-parameters))

      (unless get-name
        (push (cons :get-name
                    (byte-compile
                     `(lambda (state)
                        (push (cons 'persp-name ,name) state)
                        state)))
              auto-persp-parameters))

      (unless get-persp
        (push (cons :get-persp
                    #'(lambda (state)
                        (let ((name (alist-get 'persp-name state)))
                          (when name
                            (push (cons 'persp (persp-add-new name))
                                  state)))
                        state))
              auto-persp-parameters))

      (unless get-buffer
        (push (cons :get-buffer
                    #'(lambda (state)
                        (push (cons 'buffer (current-buffer))
                              state)
                        state))
              auto-persp-parameters))

      (unless on-match
        (push (cons :on-match
                    #'persp--auto-persp-default-on-match)
              auto-persp-parameters))

      (unless after-match
        (push (cons :after-match
                    #'persp--auto-persp-default-after-match)
              auto-persp-parameters))

      (when (or (null hooks) (not (consp hooks)))
        (unless hooks
          (setq hooks
                (when minor-mode
                  (intern (concat (symbol-name minor-mode)
                                  "-hook")))))
        (unless hooks
          (setq hooks
                (cond
                 (mode
                  (intern (concat (symbol-name mode)
                                  "-hook")))
                 (minor-mode
                  (intern (concat (symbol-name minor-mode)
                                  "-hook")))
                 ((or mode-name predicate buffer-name)
                  'after-change-major-mode-hook)
                 (file-name 'find-file-hook)
                 (t 'after-change-major-mode-hook))))

        (when (and hooks (not (consp hooks)))
          (setq hooks (list hooks)))

        (push (cons :hooks hooks) auto-persp-parameters))

      (setq generated-predicate
            (apply #'persp--generate-buffer-predicate
                   (if predicate
                       keyargs
                     (cons :true-value (cons '(car rest-args) keyargs)))))
      (push (cons :generated-predicate generated-predicate) auto-persp-parameters)

      (setq main-action
            (eval
             `(lambda (&optional buffer hook hook-args)
                (let (,@dyn-env)
                  (let* ((state (copy-alist
                                 (persp-auto-persp-parameters ,name))))
                    (push (cons 'hook hook) state)
                    (push (cons 'hook-args hook-args) state)
                    (if buffer
                        (push (cons 'buffer buffer) state)
                      (let ((get-buffer
                             (alist-get :get-buffer state)))
                        (setq state (funcall get-buffer state))))
                    (when
                        (setq state
                              (funcall (alist-get :generated-predicate state)
                                       (alist-get 'buffer state) state))
                      (with-current-buffer (alist-get 'buffer state)
                        (let ((get-name
                               (alist-get :get-name state)))
                          (setq state (funcall get-name state)))
                        (let ((get-persp
                               (alist-get :get-persp state)))
                          (setq state (funcall get-persp state)))
                        (let ((on-match (alist-get :on-match state)))
                          (when on-match
                            (setq state (funcall on-match state))
                            (let ((after-match (alist-get :after-match state)))
                              (when after-match
                                (setq state (funcall after-match state)))))))))))))
      (push (cons :main-action main-action) auto-persp-parameters)

      (when hooks
        (let ((aparams-hooks (assq :hooks auto-persp-parameters)))
          (dolist (hook hooks)
            (setq generated-hook
                  (with-no-warnings
                    (let ((warning-minimum-level :emergency)
                          byte-compile-warnings)
                      (byte-compile
                       `(lambda (&rest hook-args)
                          (when persp-mode
                            (funcall (with-no-warnings ',main-action)
                                     nil ',hook hook-args)))))))
            (setcdr aparams-hooks (delete hook (cdr aparams-hooks)))
            (push (cons hook generated-hook) (cdr aparams-hooks)))))

      (let ((auto-persp-definition (assoc name persp-auto-persp-alist)))
        (if auto-persp-definition
            (progn
              (persp-auto-persp-deactivate-hooks name)
              (setcdr auto-persp-definition auto-persp-parameters))
          (setq auto-persp-definition (cons name auto-persp-parameters))
          (push auto-persp-definition persp-auto-persp-alist)))

      (persp-auto-persp-activate-hooks name)

      (unless dont-pick-up-buffers
        (persp-auto-persp-pickup-buffers-for name)))))

;;;###autoload
(define-obsolete-function-alias 'def-auto-persp 'persp-def-auto-persp
  "persp-mode 2.9.6")


;; Custom save/load functions

;;;###autoload
(defun* def-persp-buffer-save/load
    (&rest keyargs
           &key buffer-name file-name mode mode-name minor-mode minor-mode-name
           predicate tag-symbol save-vars save-function load-function after-load-function
           append)
  (let ((generated-save-predicate
         (apply #'persp--generate-buffer-predicate keyargs))
        save-body load-fun)
    (when save-vars
      (unless (listp save-vars) (setq save-vars (list save-vars)))
      (when (and (or mode mode-name) (not (memq 'major-mode save-vars)))
        (push 'major-mode save-vars)))
    (unless tag-symbol (setq tag-symbol 'def-buffer-with-vars))

    (setq save-body
          `(let ((vars-list
                  (with-current-buffer buffer
                    (delete-if-not
                     #'(lambda (lvar)
                         ,(persp--generate-predicate-loop-any-all
                           save-vars
                           '(and
                             (if (persp-regexp-p item)
                                 (persp-string-match-p item
                                                       (symbol-name lvar))
                               (eq item lvar))
                             (persp-elisp-object-readable-p
                              (symbol-value lvar)))
                           t))
                     (buffer-local-variables)
                     :key #'car-safe))))
             ,(if save-function
                  `(funcall (with-no-warnings ',save-function)
                            buffer ',tag-symbol vars-list)
                `(list ',tag-symbol (buffer-name buffer) vars-list)))
          save-body `(when (funcall (with-no-warnings ',generated-save-predicate)
                                    buffer)
                       ,save-body))

    (setq load-fun
          `(lambda (savelist)
             (destructuring-bind
                 (buffer-name vars-list &rest _rest) (cdr savelist)
               (let ((buf-file (alist-get 'buffer-file-name vars-list))
                     (buf-mmode (alist-get 'major-mode vars-list)))
                 (lexical-let
                     ((persp-loaded-buffer
                       (persp-buffer-from-savelist
                        (list 'def-buffer buffer-name buf-file buf-mmode
                              (list (cons 'local-vars vars-list)))))
                      (persp-after-load-function (with-no-warnings ',after-load-function))
                      persp-after-load-lambda)
                   (when (and persp-loaded-buffer persp-after-load-function)
                     (setq persp-after-load-lambda
                           #'(lambda (&rest pall-args)
                               (apply persp-after-load-function
                                      persp-loaded-buffer pall-args)
                               (remove-hook 'persp-after-load-state-functions
                                            persp-after-load-lambda)))
                     (add-hook 'persp-after-load-state-functions
                               persp-after-load-lambda t))
                   persp-loaded-buffer)))))

    (add-hook 'persp-save-buffer-functions
              (eval `(lambda (buffer) ,save-body)) append)
    (add-hook 'persp-load-buffer-functions
              (eval
               `(lambda (savelist)
                  (when (eq (car savelist) ',tag-symbol)
                    (let ((default-load-fun (with-no-warnings ',load-fun)))
                      ,(if load-function
                           `(funcall (with-no-warnings ',load-function)
                                     savelist default-load-fun
                                     (with-no-warnings ',after-load-function))
                         `(funcall default-load-fun savelist))))))
              append)))


;; Mode itself:

;;;###autoload
(define-minor-mode persp-mode
  "Toggle the persp-mode.
When active, keeps track of multiple 'perspectives',
named collections of buffers and window configurations."
  :require    'persp-mode
  :group      'persp-mode
  :keymap     persp-mode-map
  :init-value nil
  :global     t
  :lighter    (:eval persp-lighter)
  (if persp-mode
      (progn
        (setq persp-special-last-buffer nil)
        (add-hook 'find-file-hook #'persp-special-last-buffer-make-current)
        (if (or noninteractive
                (and (daemonp)
                     (null (cdr (frame-list)))
                     (eq (selected-frame) terminal-frame)))
            (progn
              (add-hook 'after-make-frame-functions #'persp-mode-start-and-remove-from-make-frame-hook)
              (setq persp-mode nil))

          (setq *persp-hash* (make-hash-table :test #'equal :size 10))
          (setq persp-buffer-props-hash (make-hash-table :test #'eq :size 10))

          (push '(persp . writable) window-persistent-parameters)

          (persp-add-minor-mode-menu)
          (persp-add-new persp-nil-name)

          (add-hook 'find-file-hook              #'persp-add-or-not-on-find-file)
          (add-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function)
          (add-hook 'kill-buffer-hook            #'persp-kill-buffer-h)
          (add-hook 'before-make-frame-hook      #'persp-before-make-frame)
          (add-hook 'after-make-frame-functions  #'persp-init-new-frame)
          (add-hook 'delete-frame-functions      #'persp-delete-frame)
          (add-hook 'kill-emacs-query-functions  #'persp-kill-emacs-query-function)
          (add-hook 'kill-emacs-hook             #'persp-kill-emacs-h)
          (add-hook 'server-switch-hook          #'persp-server-switch)
          (add-hook 'after-change-major-mode-hook #'persp-after-change-major-mode-h)

          (persp-set-ido-hooks persp-set-ido-hooks)
          (persp-set-read-buffer-function persp-set-read-buffer-function)

          (persp-update-completion-system persp-interactive-completion-system)

          (condition-case-unless-debug err
              (mapc #'persp-init-frame (persp-frame-list-without-daemon))
            (error
             (message "[persp-mode] Error: Can not initialize frame -- %s"
                      err)))

          (when (fboundp 'tabbar-mode)
            (setq tabbar-buffer-list-function #'persp-buffer-list))

          (persp-auto-persps-activate-hooks)

          (if (> persp-auto-resume-time 0)
              (run-at-time persp-auto-resume-time nil
                           #'(lambda ()
                               (remove-hook 'find-file-hook #'persp-special-last-buffer-make-current)
                               (when (> persp-auto-resume-time 0)
                                 (condition-case-unless-debug err
                                     (persp-load-state-from-file)
                                   (error
                                    (message "[persp-mode] Error: Can not autoresume perspectives -- %s"
                                             err)))
                                 (when (persp-get-buffer-or-null persp-special-last-buffer)
                                   (persp-switch-to-buffer persp-special-last-buffer)))))
            (remove-hook 'find-file-hook #'persp-special-last-buffer-make-current))))

    (run-hooks 'persp-mode-deactivated-hook)
    (when (> persp-auto-save-opt 1) (persp-save-state-to-file))

    (remove-hook 'find-file-hook               #'persp-add-or-not-on-find-file)
    (remove-hook 'kill-buffer-query-functions  #'persp-kill-buffer-query-function)
    (remove-hook 'kill-buffer-hook             #'persp-kill-buffer-h)
    (remove-hook 'before-make-frame-hook       #'persp-before-make-frame)
    (remove-hook 'after-make-frame-functions   #'persp-init-new-frame)
    (remove-hook 'delete-frame-functions       #'persp-delete-frame)
    (remove-hook 'kill-emacs-query-functions   #'persp-kill-emacs-query-function)
    (remove-hook 'kill-emacs-hook              #'persp-kill-emacs-h)
    (remove-hook 'server-switch-hook           #'persp-server-switch)
    (remove-hook 'after-change-major-mode-hook #'persp-after-change-major-mode-h)

    (persp-set-ido-hooks)
    (persp-set-read-buffer-function)
    (persp-update-frames-buffer-predicate t)
    (persp-update-completion-system nil t)

    (persp-auto-persps-deactivate-hooks)

    (when (fboundp 'tabbar-mode)
      (setq tabbar-buffer-list-function #'tabbar-buffer-list))

    (setq window-persistent-parameters
          (delq (assq 'persp window-persistent-parameters)
                window-persistent-parameters))

    ;; TODO: do it properly -- remove buffers, kill perspectives
    (setq *persp-hash* nil)
    (setq persp-buffer-props-hash nil)))


;; Hooks:

(defun persp--kill-buffer-query-function-foreign-check (opt persp buf &optional base-buf)
  (let ((buf-to-check buf)
        pbcontain)
    (when (and base-buf (eq 'as-base-buffer opt))
      (setq opt persp-kill-foreign-buffer-behaviour
            buf-to-check base-buf)
      (setq pbcontain (persp-contain-buffer-p buf-to-check persp)))
    (cond
     (pbcontain t)
     ((functionp opt) (funcall opt))
     ((null opt) t)
     ((eq opt 'kill) t)
     ((and (eq 'dont-ask-weak opt)
           (persp-buffer-free-p buf-to-check t)) t)
     (t
      (let ((curwin (selected-window))
            (prompt (format "You are going to kill a buffer(%s) \
which is not in the current perspective. It will be removed from every perspective \
and then killed.\nWhat do you really want to do? "
                            (buffer-name buf))))
        (macrolet
            ((clwin (w)
                    `(run-at-time 1 nil #'(lambda (ww) (when (window-live-p ww)
                                                    (delete-window ww)))
                                  ,w))
             (swb (b w)
                  `(run-at-time 1 nil
                                #'(lambda (bb ww)
                                    (with-selected-window ww
                                      (persp-set-another-buffer-for-window bb ww)))
                                ,b ,w)))
          (destructuring-bind (char &rest _)
              (read-multiple-choice
               prompt
               '((?q "do nothing")
                 (?k "kill")
                 (?K "kill and close window")
                 (?c "close window")
                 (?s "switch to another buffer")))
            (case char
              ((?q ?\C-g ?\C-\[) nil)
              (?k t)
              (?K (clwin curwin) t)
              (?c (clwin curwin) nil)
              (?s (swb buf curwin) nil)
              (t t)))))))))

(defun persp-kill-buffer-query-function ()
  "This must be the last hook in the kill-buffer-query-functions.
Otherwise if a next function in the list returns nil -- buffer will not be killed,
but just removed from a perspective."
  (when persp-mode
    (block pkbqf
      (let ((buffer (current-buffer)))
        (when (persp--buffer-in-persps buffer)
          (let* ((persp (get-current-persp))
                 (pbcontain (persp-contain-buffer-p buffer persp))
                 (foreign-check-passed
                  (if pbcontain
                      t
                    (let* ((base-buffer (buffer-base-buffer buffer))
                           (fb-kill-behaviour
                            (if (and base-buffer
                                     (not (eq 'do-not-override
                                              persp-kill-foreign-indirect-buffer-behaviour-override)))
                                persp-kill-foreign-indirect-buffer-behaviour-override
                              persp-kill-foreign-buffer-behaviour)))
                      (if (and fb-kill-behaviour
                               (not (persp-buffer-filtered-out-p buffer)))
                          (persp--kill-buffer-query-function-foreign-check
                           fb-kill-behaviour persp buffer base-buffer)
                        t)))))
            (if foreign-check-passed
                (when (and persp (persp-buffer-in-other-p* buffer persp))
                  (if pbcontain
                      (persp-remove-buffer buffer persp nil t nil nil)
                    (persp-remove-buffer buffer nil nil t nil nil))
                  (return-from pkbqf nil))
              (return-from pkbqf nil)))))
      t)))

(defun persp-kill-buffer-h ()
  (when (and persp-mode (persp--buffer-in-persps (current-buffer)))
    (let (persp-autokill-buffer-on-remove)
      (persp-remove-buffer (current-buffer) nil t
                           persp-when-kill-switch-to-buffer-in-perspective t nil))))

(defun persp--restore-buffer-on-find-file ()
  (when (buffer-live-p persp-special-last-buffer)
    (set-window-buffer (or (get-buffer-window) (selected-window))
                       persp-special-last-buffer))
  (setq persp-special-last-buffer nil)
  (remove-hook 'window-configuration-change-hook #'persp--restore-buffer-on-find-file))
(defun persp-add-or-not-on-find-file ()
  (let ((no-select
         (not (funcall persp-backtrace-frame-function 0 'find-file))))
    (and (case persp-add-buffer-on-find-file
           ('nil nil)
           (if-not-autopersp
            (let ((ret (not (persp-buffer-match-auto-persp-p (current-buffer)))))
              (unless (or ret no-select)
                (setq persp-special-last-buffer (window-buffer))
                (add-hook 'window-configuration-change-hook #'persp--restore-buffer-on-find-file))
              ret))
           (add-but-not-switch-if-autopersp
            (when (and (not no-select)
                       (persp-buffer-match-auto-persp-p (current-buffer)))
              (setq no-select t)
              (setq persp-special-last-buffer (window-buffer))
              (add-hook 'window-configuration-change-hook #'persp--restore-buffer-on-find-file))
            t)
           (t t))
         (persp-add-buffer (current-buffer) (get-current-persp) (not no-select) nil))))

(defun persp-after-change-major-mode-h ()
  (let ((buf (current-buffer)))
    (persp-find-and-set-persps-for-buffer buf)
    (when (and (case persp-add-buffer-on-after-change-major-mode
                 ('nil nil)
                 (free (persp-buffer-free-p buf))
                 (t t))
               (not (persp-buffer-filtered-out-p
                     buf persp-add-buffer-on-after-change-major-mode-filter-functions)))
      (persp-add-buffer buf (get-current-persp) nil nil))))

(defun persp-server-switch ()
  (condition-case-unless-debug err
      (let* ((frame (selected-frame))
             (persp-server-switch-hook (frame-parameter frame 'persp-server-switch-hook)))
        (when persp-server-switch-hook
          (unless (string-match-p "^.*magit.*$" (symbol-name last-command))
            (funcall persp-server-switch-hook frame))
          (set-frame-parameter frame 'persp-server-switch-hook nil)))
    (error
     (message "[persp-mode] Error: error in server-switch-hook -- %s"
              err))))


;; Misc funcs:

(defun* persp-gen-random-name (&optional name (phash *persp-hash*))
  (unless name (setq name (number-to-string (random))))
  (macrolet ((namegen () `(format "%s:%s" name (random 9))))
    (do ((nname name (namegen)))
        ((eq persp-not-persp (persp-get-by-name nname phash persp-not-persp)) nname))))

(defsubst persp-is-frame-daemons-frame (f)
  (and (daemonp) (eq f terminal-frame)))

(defun persp-frame-list-without-daemon ()
  "Return a list of frames without the daemon's frame."
  (if (daemonp)
      (filtered-frame-list #'(lambda (f) (not (persp-is-frame-daemons-frame f))))
    (frame-list)))

(defun set-frame-persp (persp &optional frame)
  (set-frame-parameter frame 'persp persp))

(defun get-frame-persp (&optional frame)
  (frame-parameter frame 'persp))

(defun* persp-names (&optional (phash *persp-hash*) (reverse t))
  (let (ret)
    (maphash #'(lambda (k p)
                 (push k ret))
             phash)
    (if reverse
        (nreverse ret)
      ret)))

(defun set-window-persp* (persp-name &optional window)
  (when persp-name
    (set-window-parameter window 'persp persp-name)))
(defun get-window-persp* (&optional window)
  (window-parameter window 'persp))
(defun set-window-persp (persp &optional window)
  (let ((frame (window-frame window)))
    (if (eq persp (get-frame-persp frame))
        (clear-window-persp window)
      (set-window-persp* (safe-persp-name persp) window))))
(defun window-persp-set-p (&optional window)
  (get-window-persp* window))
(defun get-window-persp (&optional window)
  (let ((pn (get-window-persp* window)))
    (when pn (persp-get-by-name pn))))
(defun clear-window-persp (&optional window)
  (set-window-parameter window 'persp nil))

(defun get-current-persp (&optional frame window)
  (with-selected-frame (or frame (selected-frame))
    (if (window-persp-set-p window)
        (get-window-persp window)
      (get-frame-persp frame))))

(defun set-current-persp (persp)
  (if (window-persp-set-p)
      (set-window-persp persp)
    (set-frame-persp persp)))

(defun persp-names-current-frame-fast-ordered ()
  (or (mapcar #'caddr (cddddr persp-minor-mode-menu))
      (list persp-nil-name)))

(defun* persp-get-by-name (name &optional (phash *persp-hash*) default)
  (gethash name phash default))


(defsubst* persp-names-sorted (&optional (phash *persp-hash*))
  (sort (persp-names phash nil) #'string<))

(defun persp-group-by (keyf lst &optional reverse)
  (let (result)
    (mapc #'(lambda (pd)
              (let* ((key (funcall keyf pd))
                     (kv (assoc key result)))
                (if kv
                    (setcdr kv (cons pd (cdr kv)))
                  (push (cons key (list pd)) result))))
          lst)
    (if reverse
        (nreverse
         (mapcar #'(lambda (gr)
                     (destructuring-bind (key . pd) gr
                       (cons key (nreverse pd))))
                 result))
      result)))

(defun persp-regexp-p (obj)
  (or (stringp obj) (and (consp obj) (stringp (cdr obj)))))
(defun persp-string-match-p (regexp string &optional start)
  (when (and regexp (not (consp regexp)))
    (setq regexp (cons t regexp)))
  (let ((ret (string-match-p (cdr regexp) string start)))
    (if (eq :not (car regexp))
        (not ret)
      ret)))

(defun* persp-persps (&optional (phash *persp-hash*) &optional names-regexp reverse)
  (when (and names-regexp (not (consp names-regexp)))
    (setq names-regexp (cons t names-regexp)))
  (let (ret)
    (maphash #'(lambda (k p)
                 (if names-regexp
                     (when (persp-string-match-p names-regexp k)
                       (push p ret))
                   (push p ret)))
             phash)
    (if reverse
        (nreverse ret)
      ret)))

(defun* persp-other-not-hidden-persps (&optional persp (phash *persp-hash*))
  (delete-if #'safe-persp-hidden (delq persp (persp-persps phash))))

(defun* persp-other-persps-with-buffer-except-nil
    (&optional (buff-or-name (current-buffer)) (persp (get-current-persp))
               (phash *persp-hash*) del-weak)
  (let ((buf (persp-get-buffer-or-null buff-or-name))
        ret)
    (when buf
      (setq ret (delete-if-not
                 (apply-partially #'memq buf)
                 (delq persp (delq nil (persp-persps phash)))
                 :key #'persp-buffers))
      (when del-weak
        (setq ret (delete-if #'persp-weak ret))))
    ret))
(defun* persp-other-persps-with-buffer-except-nil*
    (&optional (buff-or-name (current-buffer)) (persp (get-current-persp)) del-weak)
  (let ((persps (persp--buffer-in-persps
                 (persp-get-buffer-or-null buff-or-name))))
    (when persp
      (setq persps (remq persp persps)))
    (when del-weak
      (setq persps (remove-if #'persp-weak persps)))
    persps))

(defun* persp-buffer-in-other-p
    (&optional (buff-or-name (current-buffer)) (persp (get-current-persp))
               (phash *persp-hash*) del-weak)
  (persp-other-persps-with-buffer-except-nil buff-or-name persp phash del-weak))
(defun* persp-buffer-in-other-p*
    (&optional (buff-or-name (current-buffer)) (persp (get-current-persp)) del-weak)
  (persp-other-persps-with-buffer-except-nil* buff-or-name persp del-weak))


(defun* persp-frames-with-persp (&optional (persp (get-frame-persp)))
  (delete-if-not (apply-partially #'eq persp)
                 (persp-frame-list-without-daemon)
                 :key #'get-frame-persp))
(defun* persp-frames-and-windows-with-persp (&optional (persp (get-current-persp)))
  (let (frames windows)
    (dolist (frame (persp-frame-list-without-daemon))
      (when (eq persp (get-frame-persp frame))
        (push frame frames))
      (dolist (window (window-list frame 'no-minibuf))
        (when (and (window-persp-set-p window)
                   (eq persp (get-window-persp window)))
          (push window windows))))
    (cons frames windows)))


(defun* persp-do-buffer-list-by-regexp (&key func regexp blist noask
                                             (rest-args nil rest-args-p))
  (interactive)
  (unless func
    (let ((fs (completing-read "What function to apply: " obarray 'functionp t)))
      (when (and fs (not (string= fs "")))
        (setq func (read fs)))))
  (when func
    (unless regexp
      (setq regexp (read-regexp "Regexp: ")))
    (when regexp
      (unless blist
        (setq blist (eval (read--expression "Buffer list expression: " "nil"))))
      (when blist
        (unless rest-args-p
          (setq rest-args (read--expression "Rest arguments: " "nil")))
        (setq blist
              (remove-if-not
               (apply-partially #'persp-string-match-p regexp)
               blist))
        (when (and blist
                   (or noask (y-or-n-p (format "Do %s on these buffers:\n%s?\n"
                                               func
                                               (mapconcat 'identity blist ", ")))))
          (mapcar #'(lambda (b) (apply func b rest-args)) blist))))))


;; Perspective funcs:

(defun persp-next ()
  "Switch to next perspective (to the right)."
  (interactive)
  (let* ((persp-list (persp-names-current-frame-fast-ordered))
         (persp-list-length (length persp-list))
         (only-perspective? (equal persp-list-length 1))
         (pos (position (safe-persp-name (get-current-persp)) persp-list)))
    (cond
     ((null pos) nil)
     (only-perspective? nil)
     ((= pos (1- persp-list-length))
      (if persp-switch-wrap (persp-switch (nth 0 persp-list))))
     (t (persp-switch (nth (1+ pos) persp-list))))))

(defun persp-prev ()
  "Switch to previous perspective (to the left)."
  (interactive)
  (let* ((persp-list (persp-names-current-frame-fast-ordered))
         (persp-list-length (length persp-list))
         (only-perspective? (equal persp-list-length 1))
         (pos (position (safe-persp-name (get-current-persp)) persp-list)))
    (cond
     ((null pos) nil)
     (only-perspective? nil)
     ((= pos 0)
      (if persp-switch-wrap
          (persp-switch (nth (1- persp-list-length) persp-list))))
     (t (persp-switch (nth (1- pos) persp-list))))))

(defun* persp-add (persp &optional (phash *persp-hash*))
  "Insert `PERSP' to `PHASH'.
If we adding to the `*persp-hash*' add entries to the mode menu.
Return `PERSP'."
  (let ((name (safe-persp-name persp)))
    (puthash name persp phash)
    (when (eq phash *persp-hash*)
      (persp-add-to-menu persp)))
  persp)

(defun* persp-remove-by-name (name &optional (phash *persp-hash*))
  "Remove a perspective with name `NAME' from `PHASH'.
Save it's state before removing.
If we removing from the `*persp-hash*' remove also the menu entries.
Switch all frames with that perspective to another one.
Return the removed perspective."
  (interactive "i")
  (unless name
    (setq name (persp-read-persp "to remove" nil
                             (and (eq phash *persp-hash*) (safe-persp-name (get-current-persp)))
                             t t)))
  (let ((persp (persp-get-by-name name phash persp-not-persp))
        (persp-to-switch persp-nil-name))
    (unless (eq persp persp-not-persp)
      (persp-save-state persp)
      (if (and (eq phash *persp-hash*) (null persp))
          (message "[persp-mode] Error: Can't remove the 'nil' perspective")
        (when (eq phash *persp-hash*)
          (persp-remove-from-menu persp)
          (destructuring-bind (frames . windows)
              (persp-frames-and-windows-with-persp persp)
            (dolist (w windows) (clear-window-persp w))
            ;;(setq persp-to-switch (or (car (persp-names phash nil)) persp-nil-name))
            (dolist (f frames)
              (persp-frame-switch persp-to-switch f))))
        (remhash name phash)))
    persp))

(defun* persp-add-new (name &optional (phash *persp-hash*))
  "Create a new perspective with the given `NAME'. Add it to `PHASH'.
Return the created perspective."
  (interactive "sA name for the new perspective: ")
  (if (and name (not (string= "" name)))
      (if (member name (persp-names phash nil))
          (persp-get-by-name name phash)
        (let ((persp (if (string= persp-nil-name name)
                         nil
                       (make-persp :name name))))
          (run-hook-with-args 'persp-created-functions persp phash)
          (persp-add persp phash)))
    (message "[persp-mode] Error: Can't create a perspective with empty name.")
    nil))

(defun persp-find-and-set-persps-for-buffer (&optional buffer-or-name)
  (setq buffer-or-name (if buffer-or-name
                           (persp-get-buffer-or-null buffer-or-name)
                         (current-buffer)))
  (mapc #'(lambda (p)
            (when p
              (persp-add-buffer buffer-or-name p nil nil)))
        (persp--buffer-in-persps buffer-or-name))
  (persp--buffer-in-persps-set
   buffer-or-name
   (delete-if-not (apply-partially #'memq buffer-or-name)
                  (delq nil (persp-persps))
                  :key #'persp-buffers)))

(defun* persp-contain-buffer-p
    (&optional (buff-or-name (current-buffer)) (persp (get-current-persp)) delweak)
  (if (and delweak (safe-persp-weak persp))
      nil
    (if persp
        (memq (persp-get-buffer-or-null buff-or-name)
              (persp-buffers persp))
      t)))
(defun* persp-contain-buffer-p*
    (&optional (buff-or-name (current-buffer)) (persp (get-current-persp)) delweak)
  (if (and delweak (safe-persp-weak persp))
      nil
    (if persp
        (memq persp (persp--buffer-in-persps
                     (persp-get-buffer-or-null buff-or-name)))
      t)))

(defun* persp-add-buffer
    (&optional buffs-or-names (persp (get-current-persp))
               (switchorno persp-switch-to-added-buffer)
               (called-interactively-p (called-interactively-p 'any)))
  (interactive "i")
  (when (and called-interactively-p current-prefix-arg)
    (setq switchorno (not switchorno)))
  (unless buffs-or-names
    (setq buffs-or-names (if called-interactively-p
                             (let ((*persp-restrict-buffers-to* 1)
                                   persp-restrict-buffers-to-if-foreign-buffer)
                               (persp-read-buffer (concat
                                                   "Add buffers to the perspective"
                                                   (and switchorno
                                                        " and switch to first added buffer")
                                                   ": ")
                                                  (current-buffer) t nil t))
                           (current-buffer))))
  (unless (listp buffs-or-names) (setq buffs-or-names (list buffs-or-names)))
  (mapc
   #'(lambda (bon)
       (let ((buffer (persp-get-buffer-or-null bon)))
         (when (and persp buffer)
           (unless (persp-contain-buffer-p buffer persp)
             (push buffer (persp-buffers persp)))
           (unless (persp-contain-buffer-p* buffer persp)
             (persp--buffer-in-persps-add buffer persp)))
         (when (and buffer switchorno (eq persp (get-current-persp)))
           (persp-switch-to-buffer buffer))
         buffer))
   buffs-or-names)
  buffs-or-names)

(defun* persp-add-buffers-by-regexp (&optional regexp (persp (get-current-persp)))
  (interactive)
  (when persp
    (persp-do-buffer-list-by-regexp
     :regexp regexp :func 'persp-add-buffer :rest-args (list persp nil)
     :blist (persp-buffer-list-restricted (selected-frame) 1))))

(defun* persp-temporarily-display-buffer
    (&optional buff-or-name (called-interactively-p (called-interactively-p 'any)))
  (interactive "i")
  (let ((persp-temporarily-display-buffer t))
    (unless buff-or-name
      (setq buff-or-name
            (if called-interactively-p
                (let ((*persp-restrict-buffers-to*
                       (if (and called-interactively-p current-prefix-arg) 0 1))
                      (persp-restrict-buffers-to-if-foreign-buffer
                       (if (= 0 *persp-restrict-buffers-to*) -1 nil)))
                  (persp-read-buffer
                   (if (= 0 *persp-restrict-buffers-to*)
                       "Remove a buffer from the perspective, but still display it: "
                     "Temporarily display a buffer, not adding it to the current perspective: ")
                   nil t))
              (current-buffer))))
    (let ((buffer (persp-get-buffer-or-null buff-or-name)))
      (when buffer
        (let ((persp (get-current-persp)))
          (when (and persp (persp-contain-buffer-p* buffer persp))
            (let (persp-autokill-buffer-on-remove
                  persp-autokill-persp-when-removed-last-buffer)
              (persp-remove-buffer buffer persp nil nil nil nil))))
        (persp-switch-to-buffer buffer t)))))

(defun* persp-remove-buffer
    (&optional buffs-or-names (persp (get-current-persp)) noask-to-remall (switch t)
               called-from-kill-buffer-hook
               (called-interactively-p (called-interactively-p 'any)))
  "Remove a buffer from a perspective. Switch all windows displaying that buffer
to another one. If `PERSP' is nil -- remove the buffer from all perspectives.
Return removed buffers."
  (interactive "i")
  (unless (listp buffs-or-names) (setq buffs-or-names (list buffs-or-names)))
  (unless buffs-or-names
    (setq buffs-or-names
          (if called-interactively-p
              (let ((*persp-restrict-buffers-to* 0)
                    persp-restrict-buffers-to-if-foreign-buffer)
                (persp-read-buffer "Remove buffers from the perspective: "
                                   (current-buffer) t nil t))
            (current-buffer))))
  (mapc
   #'(lambda (bon)
       (let ((buffer (persp-get-buffer-or-null bon)))
         (when buffer
           (if (null persp)
               (when (or noask-to-remall
                         (yes-or-no-p (concat "Remove the " (buffer-name buffer)
                                              " buffer from all perspectives?")))
                 (mapc #'(lambda (p)
                           (persp-remove-buffer buffer p nil switch
                                                called-from-kill-buffer-hook called-interactively-p))
                       (persp-other-persps-with-buffer-except-nil buffer persp)))
             (persp--buffer-in-persps-remove buffer persp)
             (when (memq buffer (persp-buffers persp))
               (setf (persp-buffers persp) (delq buffer (persp-buffers persp)))
               (when switch
                 (persp-switch-to-prev-buffer buffer persp))))
           (unless called-from-kill-buffer-hook
             (let ((persp-autokill-buffer-on-remove
                    (if (and called-interactively-p current-prefix-arg)
                        (not persp-autokill-buffer-on-remove)
                      persp-autokill-buffer-on-remove)))
               (when (and persp-autokill-buffer-on-remove
                          (persp-buffer-free-p
                           buffer (eq 'kill-weak persp-autokill-buffer-on-remove)))
                 (let (persp-kill-foreign-buffer-behaviour)
                   (setq persp-autokill-buffer-on-remove nil)
                   (persp-kill-buffer buffer)))))
           (persp--do-auto-action-if-needed persp))))
   buffs-or-names)
  buffs-or-names)

(defun persp-kill-buffer (&optional buffers-or-names)
  "Kill buffers, read buffer with restriction to current perspective."
  (interactive (list
                (let ((*persp-restrict-buffers-to* 0)
                      persp-restrict-buffers-to-if-foreign-buffer)
                  (if persp-mode
                      (persp-read-buffer "Kill buffers: " (current-buffer) t nil t)
                    (read-buffer "Kill buffer: " (current-buffer) t)))))
  (unless (listp buffers-or-names) (setq buffers-or-names (list buffers-or-names)))
  (mapc #'kill-buffer
        (remove-if-not #'persp-get-buffer-or-null buffers-or-names))
  buffers-or-names)

(defun persp-switch-to-buffer (buffer-or-name &optional norecord force-same-window)
  "Switch to buffer, read buffer with restriction to current perspective."
  (interactive (list
                (let ((*persp-restrict-buffers-to* 0)
                      persp-restrict-buffers-to-if-foreign-buffer)
                  (if persp-mode
                      (persp-read-buffer "Switch to buffer: " (current-buffer) t)
                    (read-buffer "Switch to buffer: " (current-buffer) t)))))
  (when (and buffer-or-name
             (persp-get-buffer-or-null (get-buffer buffer-or-name)))
    (switch-to-buffer buffer-or-name norecord force-same-window)))

(defun* persp-remove-buffers-by-regexp (&optional regexp (persp (get-current-persp)))
  (interactive)
  (when persp
    (persp-do-buffer-list-by-regexp :regexp regexp :func 'persp-remove-buffer
                                    :blist (persp-buffers persp) :rest-args (list persp))))

(defun* persp-import-buffers-from (persp-from
                                   &optional (persp-to (get-current-persp)))
  (if persp-to
      (mapc #'(lambda (b) (persp-add-buffer b persp-to nil nil))
            (safe-persp-buffers persp-from))
    (message "[persp-mode] Error: Can't import buffers to the 'nil' perspective, \
cause it already contain all buffers.")))

(defun* persp-import-buffers
    (names
     &optional (persp-to (get-current-persp)) (phash *persp-hash*))
  "Import buffers from perspectives with the given names to another one."
  (interactive "i")
  (unless (listp names) (setq names (list names)))
  (unless names
    (setq names (persp-read-persp "to import buffers from" t nil t nil t)))
  (mapc #'(lambda (persp-from)
            (persp-import-buffers-from persp-from persp-to))
        (mapcar #'(lambda (pn) (persp-get-by-name pn phash)) names)))

(defun* persp-import-win-conf
    (name
     &optional (persp-to (get-current-persp)) (phash *persp-hash*)
     no-update-frames)
  (interactive "i")
  (unless name
    (setq name (persp-read-persp "to import window configuration from" nil nil t nil t)))
  (let ((persp-from (persp-get-by-name name phash persp-not-persp)))
    (unless (or (eq persp-to persp-from)
                (eq persp-from persp-not-persp))
      (if persp-to
          (setf (persp-window-conf persp-to) (safe-persp-window-conf persp-from))
        (setq persp-nil-wconf (persp-window-conf persp-from)))
      (unless no-update-frames
        (persp-update-frames-window-confs (list (safe-persp-name persp-to)))))))

(defun* persp-copy (new-name &optional switch (called-interactively-p (called-interactively-p 'any)))
  (interactive "i")
  (unless new-name
    (setq new-name
          (read-string "Copy current persp with name: ")))
  (if (member new-name (persp-names))
      (progn (message "[persp-mode] Error: There is already a perspective with that name %s"
                      new-name)
             nil)
    (let* ((current-persp (get-current-persp))
           (choosen-buffers t)
           (new-buffers (if (and current-persp (not (and called-interactively-p current-prefix-arg)))
                            (append (persp-buffers current-persp) nil)
                          (delete-if-not
                           (destructuring-bind (char &rest _)
                               (read-multiple-choice
                                "What buffers to copy? "
                                '((?a "all")
                                  (?d "displayed")
                                  (?f "free and displayed")
                                  (?F "free")
                                  (?c "choose")
                                  (?n "none")))
                             (case char
                               (?d #'(lambda (b) (get-buffer-window-list b 'no-minibuf)))
                               (?f #'(lambda (b) (or (persp-buffer-free-p b t)
                                                (get-buffer-window-list b 'no-minibuf))))
                               (?F #'(lambda (b) (persp-buffer-free-p b t)))
                               (?c #'(lambda (b)
                                       (unless (listp choosen-buffers)
                                         (setq choosen-buffers
                                               (persp-read-buffer "" (current-buffer) t nil t 'push)))
                                       nil))
                               (?n #'not)
                               (?a nil)
                               (t nil)))
                           (if current-persp
                               (append (persp-buffers current-persp) nil)
                             (safe-persp-buffers current-persp)))))
           (new-persp (persp-add-new new-name)))
      (when new-persp
        (persp-save-state current-persp)
        (setf (persp-window-conf new-persp) (safe-persp-window-conf current-persp)
              (persp-parameters new-persp) (append (safe-persp-parameters current-persp) nil)
              (persp-weak new-persp) (if current-persp (persp-weak current-persp) nil))
        (when (listp choosen-buffers)
          (persp-add-buffer choosen-buffers new-persp nil nil))
        (case switch
          (window (persp-window-switch new-name))
          (frame (persp-frame-switch new-name))
          (no-switch nil)
          (t (persp-switch new-name)))
        new-persp))))

(defun* persp-get-buffer (&optional (buff-or-name (current-buffer)) (persp (get-current-persp)))
  "Like `get-buffer', but constrained to the perspective's list of buffers.
Return the buffer if it's in the perspective or the first buffer from the
perspective buffers or nil."
  (let ((buffer (persp-get-buffer-or-null buff-or-name)))
    (or (find buffer (safe-persp-buffers persp))
        (first (safe-persp-buffers persp)))))

(defun persp-get-buffer-or-null (buff-or-name)
  "Safely return a buffer or the nil without errors."
  (typecase buff-or-name
    ((or string buffer)
     (let ((buf (get-buffer buff-or-name)))
       (and (buffer-live-p buf)
            buf)))
    (otherwise nil)))

(defun persp-buffer-filtered-out-p (buff-or-name &rest filters)
  (setq filters (if filters
                    (cons
                     persp-common-buffer-filter-functions
                     filters)
                  persp-common-buffer-filter-functions)
        buff-or-name (get-buffer buff-or-name))
  (find-if #'(lambda (filter)
               (if (functionp filter)
                   (funcall filter buff-or-name)
                 (find-if #'(lambda (f) (funcall f buff-or-name)) filter)))
           filters))

(defun persp-buffer-free-p (&optional buff-or-name del-weak)
  (unless buff-or-name (setq buff-or-name (current-buffer)))
  (let ((persps (persp--buffer-in-persps
                 (persp-get-buffer-or-null buff-or-name))))
    (if persps
        (if del-weak
            (not
             (find-if-not #'persp-weak persps))
          nil)
      t)))


(defun* persp-set-another-buffer-for-window
    (&optional (old-buff-or-name (current-buffer)) (window (selected-window))
               (persp (get-current-persp nil window)))
  (let ((new-buf (when persp-set-frame-buffer-predicate
                   (switch-to-prev-buffer window))))
    (if new-buf
        new-buf
      (let* ((old-buf (persp-get-buffer-or-null old-buff-or-name))
             (p-bs (safe-persp-buffers persp))
             (buffers (delete-if #'(lambda (bc)
                                     (or
                                      (and (bufferp bc) (eq bc old-buf))
                                      (eq (car bc) old-buf)
                                      (not (find (car bc) p-bs))))
                                 (append (window-prev-buffers window)
                                         (window-next-buffers window)))))
        (set-window-buffer window
                           (or (persp-get-buffer (and buffers (car (first buffers))) persp)
                               (car (persp-buffer-list-restricted (window-frame window) 2.5))
                               (car (buffer-list))))))))

(defun* persp-switch-to-prev-buffer
    (&optional (old-buff-or-name (current-buffer)) (persp (get-current-persp)))
  "Switch all windows in all frames with a perspective displaying that buffer
to some previous buffer in the perspective.
Return that old buffer."
  (let ((old-buf (persp-get-buffer-or-null old-buff-or-name)))
    (destructuring-bind (frames . windows)
        (persp-frames-and-windows-with-persp persp)
      (dolist (w windows)
        (persp-set-another-buffer-for-window old-buf w))
      (dolist (f frames)
        (dolist (w (get-buffer-window-list old-buf 'no-minibuf f))
          (persp-set-another-buffer-for-window old-buf w))))
    old-buf))

(defsubst* persp-filter-out-bad-buffers (&optional (persp (get-current-persp)))
  ;; filter out killed buffers
  (when persp
    (setf (persp-buffers persp)
          (delete-if-not #'persp-get-buffer-or-null (persp-buffers persp)))))

(defun persp-hide (names)
  (interactive "i")
  (unless (listp names) (setq names (list names)))
  (unless names
    (setq names (persp-read-persp "to hide" t (safe-persp-name (get-current-persp)) t)))
  (let ((persp-to-switch (get-current-persp))
        (hidden-persps
         (mapcar #'(lambda (pn)
                     (let ((persp (persp-get-by-name pn *persp-hash* persp-not-persp)))
                       (unless (eq persp persp-not-persp)
                         (if persp
                             (setf (persp-hidden persp) t)
                           (setq persp-nil-hidden t)))
                       persp))
                 names)))
    (when (safe-persp-hidden persp-to-switch)
      (setq persp-to-switch (car (persp-other-not-hidden-persps persp-to-switch))))
    (mapc #'(lambda (p)
              (unless (eq p persp-not-persp)
                (destructuring-bind (frames . windows)
                    (persp-frames-and-windows-with-persp p)
                  (dolist (w windows) (clear-window-persp w))
                  (dolist (f frames)
                    (persp-frame-switch (safe-persp-name persp-to-switch) f)))))
          hidden-persps)))

(defun persp-unhide (names)
  (interactive "i")
  (unless (listp names) (setq names (list names)))
  (unless names
    (let ((hidden-persps
           (mapcar #'safe-persp-name
                   (delete-if-not #'safe-persp-hidden
                                  (persp-persps)))))
      (setq names
            (persp-read-persp "to unhide" t (car hidden-persps) t nil nil hidden-persps t))))
  (when names
    (mapc #'(lambda (pn)
              (let ((persp (persp-get-by-name pn *persp-hash* persp-not-persp)))
                (unless (eq persp persp-not-persp)
                  (if persp
                      (setf (persp-hidden persp) nil)
                    (setq persp-nil-hidden nil)))))
          names)))

(defun* persp-kill (names &optional dont-kill-buffers
                          (called-interactively-p (called-interactively-p 'any)))
  (interactive "i")
  (when (and called-interactively-p current-prefix-arg)
    (setq dont-kill-buffers (not dont-kill-buffers)))
  (unless (listp names) (setq names (list names)))
  (unless names
    (setq names (persp-read-persp (concat "to kill"
                                          (and dont-kill-buffers " not killing buffers"))
                                  t (safe-persp-name (get-current-persp)) t)))
  (mapc #'(lambda (pn)
            (let ((persp (persp-get-by-name pn *persp-hash* persp-not-persp)))
              (unless (eq persp persp-not-persp)
                (when (or (not called-interactively-p)
                          (not (null persp))
                          (yes-or-no-p "Really kill the 'nil' perspective (It'l kill all buffers)?"))
                  (let ((pfile (persp-parameter 'persp-file persp)))
                    (case persp-auto-save-persps-to-their-file-before-kill
                      (persp-file nil)
                      ('nil (setq pfile nil))
                      (t (unless pfile
                           (setq pfile persp-auto-save-fname))))
                    (when pfile
                      (persp-save-to-file-by-names
                       pfile *persp-hash* (list pn) t nil)))
                  (run-hook-with-args 'persp-before-kill-functions persp)
                  (let (persp-autokill-persp-when-removed-last-buffer)
                    (if dont-kill-buffers
                        (let (persp-autokill-buffer-on-remove)
                          (mapc #'(lambda (b) (persp-remove-buffer b persp t t nil nil))
                                (safe-persp-buffers persp)))
                      (mapc #'(lambda (b) (persp-remove-buffer b persp t t nil nil))
                            (safe-persp-buffers persp))))
                  (when persp
                    (persp-remove-by-name pn))))))
        names))

(defun persp-kill-without-buffers (names)
  (interactive "i")
  (persp-kill names t nil))

(defun* persp-save-and-kill (names &optional dont-kill-buffers
                                   (called-interactively-p (called-interactively-p 'any)))
  (interactive "i")
  (when (and called-interactively-p current-prefix-arg)
    (setq dont-kill-buffers (not dont-kill-buffers)))
  (unless (listp names) (setq names (list names)))
  (unless names
    (setq names (persp-read-persp (concat "to save and kill"
                                          (and dont-kill-buffers " not killing buffers"))
                                  t (safe-persp-name (get-current-persp)) t)))
  (let ((temphash (make-hash-table :test 'equal :size 10)))
    (mapc #'(lambda (p)
              (persp-add p temphash))
          (mapcar #'(lambda (pn) (persp-get-by-name pn *persp-hash*)) names))
    (persp-save-state-to-file persp-auto-save-fname temphash
                              persp-auto-save-persps-to-their-file
                              'yes)))

(defun* persp-rename (new-name
                      &optional (persp (get-current-persp)) (phash *persp-hash*))
  "Change the name field of the `PERSP', returns old name on success, otherwise returns nil."
  (interactive "i")
  (if persp
      (let ((opersp (gethash new-name phash))
            (old-name (safe-persp-name persp)))
        (unless new-name
          (setq new-name (read-string (concat "New name for the " old-name " perspective: "))))
        (if (and (not opersp) new-name (not (string= old-name new-name)))
            (progn
              (persp-remove-from-menu persp)
              (remhash old-name phash)
              (setf (persp-name persp) new-name)
              (puthash new-name persp phash)
              (persp-add-to-menu persp)
              (run-hook-with-args 'persp-renamed-functions persp old-name new-name)
              old-name)
          (message "[persp-mode] Error: There is already a perspective with \
that name: %s." new-name)
          nil))
    (message "[persp-mode] Error: You can't rename the `nil' perspective, use \
M-x: customize-variable RET persp-nil-name RET")
    nil))

(defun* persp-switch (name &optional frame (window (selected-window))
                           (called-interactively-p (called-interactively-p 'any)))
  "Switch to the perspective with name `NAME'.
If there is no perspective with that name it will be created.
Return `NAME'."
  (interactive "i")
  (let ((switch-type 'frame))
    (if (or (window-persp-set-p window)
            (and called-interactively-p current-prefix-arg))
        (setq switch-type 'window)
      (unless frame (setq frame (window-frame window))))
    (if (eq 'window switch-type)
        (persp-window-switch name window)
      (persp-frame-switch name frame))))
(defun* persp-frame-switch (name &optional (frame (selected-frame)))
  (interactive "i")
  (unless name
    (setq name (persp-read-persp "to switch(in frame)" nil nil nil nil t)))
  (unless (memq frame persp-inhibit-switch-for)
    (run-hook-with-args 'persp-before-switch-functions name frame)
    (let ((persp-inhibit-switch-for (cons frame persp-inhibit-switch-for)))
      (persp-activate (persp-add-new name) frame)))
  name)
(defun* persp-window-switch (name &optional (window (selected-window)))
  (interactive "i")
  (unless name
    (setq name (persp-read-persp "to switch(in window)" nil nil nil nil t)))
  (unless (memq window persp-inhibit-switch-for)
    (run-hook-with-args 'persp-before-switch-functions name window)
    (let ((persp-inhibit-switch-for (cons window persp-inhibit-switch-for)))
      (persp-activate (persp-add-new name) window)))
  name)

(defun persp-before-make-frame ()
  (let ((persp (gethash (or (and persp-set-last-persp-for-new-frames
                                 persp-last-persp-name)
                            persp-nil-name) *persp-hash* persp-not-persp)))
    (when (eq persp persp-not-persp)
      (when persp-set-last-persp-for-new-frames
        (setq persp-last-persp-name persp-nil-name))
      (setq persp (persp-add-new persp-nil-name)))
    (persp-save-state persp nil t)))

(defun persp--do-auto-action-if-needed (persp)
  (when (and (safe-persp-auto persp)
             persp-autokill-persp-when-removed-last-buffer
             (null (safe-persp-buffers persp)))
    (cond
     ((functionp persp-autokill-persp-when-removed-last-buffer)
      (funcall persp-autokill-persp-when-removed-last-buffer persp))
     ((or
       (eq 'hide persp-autokill-persp-when-removed-last-buffer)
       (and (eq 'hide-auto persp-autokill-persp-when-removed-last-buffer)
            (safe-persp-auto persp)))
      (persp-hide (safe-persp-name persp)))
     ((or
       (eq t persp-autokill-persp-when-removed-last-buffer)
       (eq 'kill persp-autokill-persp-when-removed-last-buffer)
       (and
        (eq 'kill-auto persp-autokill-persp-when-removed-last-buffer)
        (safe-persp-auto persp)))
      (persp-kill (safe-persp-name persp) nil nil)))))

(defsubst persp--deactivate (frame-or-window &optional new-persp)
  (let (persp)
    (typecase frame-or-window
      (frame
       (setq persp (get-frame-persp frame-or-window))
       (unless (eq persp new-persp)
         (with-selected-frame frame-or-window
           (run-hook-with-args 'persp-before-deactivate-functions 'frame))
         (persp-frame-save-state frame-or-window
                                 (if persp-set-last-persp-for-new-frames
                                     (string= (safe-persp-name persp) persp-last-persp-name)
                                   (null persp)))))
      (window
       (setq persp (get-window-persp frame-or-window))
       (unless (eq persp new-persp)
         (with-selected-window frame-or-window
           (run-hook-with-args 'persp-before-deactivate-functions 'window)))))
    (let ((persp-inhibit-switch-for (cons frame-or-window persp-inhibit-switch-for)))
      (persp--do-auto-action-if-needed persp))))

(defun* persp-activate (persp
                        &optional (frame-or-window (selected-frame)) new-frame-p)
  (when frame-or-window
    (let (old-persp type)
      (typecase frame-or-window
        (frame
         (setq old-persp (get-frame-persp frame-or-window)
               type 'frame))
        (window
         (setq old-persp (get-window-persp frame-or-window)
               type 'window)))
      (when  (or new-frame-p
                 (not (eq old-persp persp)))
        (unless new-frame-p
          (persp--deactivate frame-or-window persp))
        (case type
          (frame
           (setq persp-last-persp-name (safe-persp-name persp))
           (set-frame-persp persp frame-or-window)
           (when persp-init-frame-behaviour
             (persp-restore-window-conf frame-or-window persp new-frame-p))
           (with-selected-frame frame-or-window
             (run-hook-with-args 'persp-activated-functions 'frame)))
          (window
           (set-window-persp persp frame-or-window)
           (let ((cbuf (window-buffer frame-or-window)))
             (unless (persp-contain-buffer-p cbuf persp)
               (persp-set-another-buffer-for-window cbuf frame-or-window persp)))
           (with-selected-window frame-or-window
             (run-hook-with-args 'persp-activated-functions 'window))))))))

(defun persp-init-new-frame (frame)
  (condition-case-unless-debug err
      (persp-init-frame frame t (frame-parameter frame 'client))
    (error
     (message "[persp-mode] Error: Can not initialize frame -- %s"
              err))))
(defun* persp-init-frame (frame &optional new-frame-p client)
  (let ((persp-init-frame-behaviour
         (cond
          ((and client (not (eq -1 persp-emacsclient-init-frame-behaviour-override)))
           persp-emacsclient-init-frame-behaviour-override)
          ((and (eq this-command 'make-frame)
                (not (eq -1 persp-interactive-init-frame-behaviour-override)))
           persp-interactive-init-frame-behaviour-override)
          ((and new-frame-p (not (eq -1 persp-init-new-frame-behaviour-override)))
           persp-init-new-frame-behaviour-override)
          (t persp-init-frame-behaviour))))
    (let (persp-name persp)
      (macrolet ((set-default-persp
                  () `(progn
                        (setq persp-name (or (and persp-set-last-persp-for-new-frames
                                                  persp-last-persp-name)
                                             persp-nil-name)
                              persp (persp-get-by-name persp-name *persp-hash* persp-not-persp))
                        (when (eq persp persp-not-persp)
                          (setq persp-name persp-nil-name
                                persp (persp-add-new persp-name))))))
        (typecase persp-init-frame-behaviour
          (function
           (funcall persp-init-frame-behaviour frame new-frame-p))
          (string
           (setq persp-name persp-init-frame-behaviour
                 persp (persp-add-new persp-name)))
          (symbol
           (case persp-init-frame-behaviour
             (auto-temp (setq persp-name (persp-gen-random-name)
                              persp (persp-add-new persp-name))
                        (when persp
                          (setf (persp-auto persp) t)))
             (prompt (select-frame frame)
                     (setq persp-name
                           (persp-read-persp "to switch" nil nil nil nil t)
                           persp (persp-add-new persp-name)))
             (t (set-default-persp))))
          (t (set-default-persp))))
      (when persp-name
        (modify-frame-parameters frame `((persp . nil)))
        (when persp-set-frame-buffer-predicate
          (persp-set-frame-buffer-predicate frame))
        (persp-set-frame-server-switch-hook frame)
        (when (or (eq persp-init-frame-behaviour 'persp-ignore-wconf)
                  (eq persp-init-frame-behaviour 'persp-ignore-wconf-once))
          (set-frame-parameter frame persp-init-frame-behaviour t))
        (persp-activate persp frame new-frame-p)))))

(defun persp-delete-frame (frame)
  (condition-case-unless-debug err
      (persp--deactivate frame persp-not-persp)
    (error
     (message "[persp-mode] Error: Can not deactivate frame -- %s"
              err))))

(defun* find-other-frame-with-persp (&optional (persp (get-frame-persp))
                                               (exframe (selected-frame))
                                               for-save)
  (let ((flist (delq exframe (persp-frames-with-persp persp))))
    (find-if
     #'(lambda (f)
         (and f
              (if for-save
                  (and (not (frame-parameter f 'persp-ignore-wconf))
                       (not (frame-parameter f 'persp-ignore-wconf-once)))
                t)
              (eq persp (get-frame-persp f))))
     flist)))


;; Helper funcs:

(defun persp-add-minor-mode-menu ()
  (easy-menu-define persp-minor-mode-menu
    persp-mode-map
    "The menu for the `persp-mode'."
    '("Perspectives"
      "-")))

(defun persp-remove-from-menu (persp)
  (easy-menu-remove-item persp-minor-mode-menu nil (safe-persp-name persp))
  (when persp
    (easy-menu-remove-item persp-minor-mode-menu '("kill") (persp-name persp))))

(defun persp-add-to-menu (persp)
  (let ((name (safe-persp-name persp)))
    (lexical-let ((str_name name))
      (easy-menu-add-item persp-minor-mode-menu nil
                          (vector str_name #'(lambda () (interactive)
                                               (persp-switch str_name))))
      (when persp
        (easy-menu-add-item persp-minor-mode-menu '("kill")
                            (vector str_name #'(lambda () (interactive)
                                                 (persp-kill str_name))))))))

(defun* persp-read-persp
    (&optional action multiple default require-match delnil delcur persp-list show-hidden (default-mode t))
  (let ((persps (or persp-list
                    (persp-names-current-frame-fast-ordered))))
    (when delnil
      (setq persps (delete persp-nil-name persps)))
    (when delcur
      (setq persps (delete (safe-persp-name (get-current-persp)) persps)))
    (unless show-hidden
      (setq persps (delete-if #'safe-persp-hidden persps :key #'persp-get-by-name)))
    (when (and default (not (member default persps)))
      (setq default nil))
    (let (retlst)
      (macrolet ((call-pif ()
                           `(funcall persp-interactive-completion-function
                                     (concat
                                      "Perspective name" (and multiple "s") (and action " ") action
                                      (if default (concat " (default " default ")") "")
                                      (when retlst
                                        (concat "< " (mapconcat #'identity retlst " ") " > "))
                                      ": ")
                                     persps nil require-match nil nil default)))
        (if multiple
            (let ((done_str "[>done<]") (not-finished default-mode)
                  exit-minibuffer-function mb-local-key-map
                  (push-keys (alist-get 'push-item persp-read-multiple-keys))
                  (pop-keys (alist-get 'pop-item persp-read-multiple-keys))
                  push-keys-backup pop-keys-backup)
              (while (member done_str persps)
                (setq done_str (concat ">" done_str)))
              (let ((persp-minibuffer-setup
                     #'(lambda ()
                         (setq mb-local-key-map (current-local-map))
                         (when (keymapp mb-local-key-map)
                           (unless exit-minibuffer-function
                             (setq exit-minibuffer-function
                                   (or (lookup-key mb-local-key-map (kbd "RET"))
                                       persp-read-multiple-exit-minibuffer-function)))
                           (unless push-keys-backup
                             (setq push-keys-backup
                                   (lookup-key mb-local-key-map push-keys)))
                           (define-key mb-local-key-map push-keys
                             #'(lambda () (interactive)
                                 (setq not-finished 'push)
                                 (funcall exit-minibuffer-function)))
                           (unless pop-keys-backup
                             (setq pop-keys-backup
                                   (lookup-key mb-local-key-map pop-keys)))
                           (define-key mb-local-key-map pop-keys
                             #'(lambda () (interactive)
                                 (setq not-finished 'pop)
                                 (funcall exit-minibuffer-function))))))
                    cp)
                (unwind-protect
                    (progn
                      (add-hook 'minibuffer-setup-hook persp-minibuffer-setup t)
                      (while not-finished
                        (setq cp (call-pif))
                        (case not-finished
                          (push
                           (when (and cp (member cp persps))
                             (if retlst
                                 (when (string= cp done_str)
                                   (setq not-finished nil))
                               (push done_str persps))
                             (when not-finished
                               (if (eq 'reverse multiple)
                                   (setq retlst (append retlst (list cp)))
                                 (push cp retlst))
                               (setq persps (delete cp persps)
                                     default done_str)))
                           (when not-finished
                             (setq not-finished default-mode)))
                          (pop
                           (let ((last-item (pop retlst)))
                             (unless retlst (setq persps (delete done_str persps)
                                                  default nil))
                             (when last-item
                               (push last-item persps)))
                           (setq not-finished default-mode))
                          (t
                           (when (and cp (not (string= cp done_str)) (member cp persps))
                             (push cp retlst))
                           (setq not-finished nil)))))
                  (remove-hook 'minibuffer-setup-hook persp-minibuffer-setup)
                  (when (keymapp mb-local-key-map)
                    (when (lookup-key mb-local-key-map push-keys)
                      (define-key mb-local-key-map push-keys push-keys-backup))
                    (when (lookup-key mb-local-key-map pop-keys)
                      (define-key mb-local-key-map pop-keys pop-keys-backup)))))
              retlst)
          (call-pif))))))
(define-obsolete-function-alias 'persp-prompt 'persp-read-persp "persp-mode 2.9")

(defsubst persp--set-frame-buffer-predicate-buffer-list-cache (buflist)
  (prog1
      (setq persp-frame-buffer-predicate-buffer-list-cache buflist)
    (unless persp-frame-buffer-predicate-buffer-list-cache
      (setq persp-frame-buffer-predicate-buffer-list-cache :nil))
    (run-at-time 2 nil #'(lambda () (setq persp-frame-buffer-predicate-buffer-list-cache nil)))))
(defmacro persp--get-frame-buffer-predicate-buffer-list-cache (buflist)
  `(if persp-frame-buffer-predicate-buffer-list-cache
       (if (eq :nil persp-frame-buffer-predicate-buffer-list-cache)
           nil
         persp-frame-buffer-predicate-buffer-list-cache)
     (persp--set-frame-buffer-predicate-buffer-list-cache ,buflist)))
(defun persp-generate-frame-buffer-predicate (opt)
  (if opt
      (eval
       `(lambda (b)
          (if (string-prefix-p " *temp*" (buffer-name (current-buffer)))
              t
            ,(typecase opt
               (function
                `(funcall (with-no-warnings ',opt) b))
               (number
                `(let ((*persp-restrict-buffers-to* ,opt))
                   (memq b
                         (persp--get-frame-buffer-predicate-buffer-list-cache
                          (let ((ret (persp-buffer-list-restricted
                                      (selected-frame) ,opt
                                      persp-restrict-buffers-to-if-foreign-buffer t)))
                            (if (get-current-persp)
                                ret
                              (delete-if #'persp-buffer-filtered-out-p ret)))))))
               (symbol
                (case opt
                  ('nil t)
                  (restricted-buffer-list
                   '(progn
                      (memq b
                            (persp--get-frame-buffer-predicate-buffer-list-cache
                             (let ((ret (persp-buffer-list-restricted
                                         (selected-frame)
                                         *persp-restrict-buffers-to*
                                         persp-restrict-buffers-to-if-foreign-buffer
                                         t)))
                               (if (get-current-persp)
                                   ret
                                 (delete-if #'persp-buffer-filtered-out-p ret)))))))
                  (t '(memq b (persp--get-frame-buffer-predicate-buffer-list-cache
                               (let ((ret (safe-persp-buffers (get-current-persp))))
                                 (if (get-current-persp)
                                     ret
                                   (delete-if #'persp-buffer-filtered-out-p ret))))))))
               (t t)))))
    nil))

(defun persp-set-frame-buffer-predicate (frame &optional off)
  (let ((old-pred (frame-parameter frame 'persp-buffer-predicate-old))
        (cur-pred (frame-parameter frame 'buffer-predicate))
        (last-persp-pred (frame-parameter frame 'persp-buffer-predicate-generated)))
    (let (new-pred)
      (if off
          (progn
            (set-frame-parameter frame 'persp-buffer-predicate-old nil)
            (set-frame-parameter frame 'persp-buffer-predicate-generated nil)
            (setq new-pred (if (eq cur-pred last-persp-pred) old-pred cur-pred))
            (set-frame-parameter frame 'buffer-predicate new-pred))
        (unless persp-frame-buffer-predicate
          (setq persp-frame-buffer-predicate
                (persp-generate-frame-buffer-predicate
                 persp-set-frame-buffer-predicate)))
        (if persp-frame-buffer-predicate
            (progn
              (set-frame-parameter frame 'persp-buffer-predicate-old
                                   (if (eq cur-pred last-persp-pred)
                                       old-pred (setq old-pred cur-pred)))
              (setq new-pred
                    (case old-pred
                      ('nil persp-frame-buffer-predicate)
                      (t `(lambda (b)
                            (and
                             (funcall (with-no-warnings ',persp-frame-buffer-predicate) b)
                             (funcall (with-no-warnings ',old-pred) b))))))
              (unless (symbolp new-pred)
                (setq new-pred (with-no-warnings
                                 (let ((warning-minimum-level :emergency)
                                       byte-compile-warnings)
                                   (byte-compile new-pred)))))
              (set-frame-parameter frame 'persp-buffer-predicate-generated new-pred)
              (set-frame-parameter frame 'buffer-predicate new-pred))
          (persp-set-frame-buffer-predicate frame t))))))

(defun persp-update-frames-buffer-predicate (&optional off)
  (unless off
    (setq persp-frame-buffer-predicate nil)
    (persp-update-frames-buffer-predicate t))
  (mapc #'(lambda (f) (persp-set-frame-buffer-predicate f off))
        (persp-frame-list-without-daemon)))


(defun persp-generate-frame-server-switch-hook (opt)
  (if opt
      (eval
       `(lambda (frame)
          ,(if (functionp opt)
               `(funcall (with-no-warnings ',opt) frame)
             `(let* ((frame-client (frame-parameter frame 'client))
                     (frame-client-bl (when (processp frame-client)
                                        (process-get frame-client 'buffers))))
                ,(case opt
                   (only-file-windows
                    `(if frame-client
                         (when frame-client-bl
                           (mapc #'(lambda (w)
                                     (unless (memq (window-buffer w) frame-client-bl)
                                       (delete-window w)))
                                 (window-list frame 'no-minibuf)))
                       (let (frame-server-bl)
                         (mapc #'(lambda (proc)
                                   (setq frame-server-bl
                                         (append frame-server-bl (process-get proc 'buffers))))
                               (server-clients-with 'frame nil))
                         (when frame-server-bl
                           (mapc #'(lambda (w)
                                     (unless (memq (window-buffer w) frame-server-bl)
                                       (delete-window w)))
                                 (window-list frame 'no-minibuf))))))
                   (only-file-windows-for-client-frame
                    `(when frame-client-bl
                       (mapc #'(lambda (w)
                                 (unless (memq (window-buffer w) frame-client-bl)
                                   (delete-window w)))
                             (window-list frame 'no-minibuf))))
                   (t nil))))))
    nil))

(defun persp-set-frame-server-switch-hook (frame)
  (when (frame-parameter frame 'client)
    (set-frame-parameter frame 'persp-server-switch-hook persp-frame-server-switch-hook)))

(defun persp-update-frame-server-switch-hook ()
  (setq persp-frame-server-switch-hook
        (persp-generate-frame-server-switch-hook persp-server-switch-behaviour))
  (mapc #'persp-set-frame-server-switch-hook
        (persp-frame-list-without-daemon)))


(defun persp-ido-setup ()
  (when (eq ido-cur-item 'buffer)
    (setq persp-disable-buffer-restriction-once nil)))

(defun persp-restrict-ido-buffers ()
  "Support for the `ido-mode'."
  (let ((buffer-names-sorted
         (if persp-disable-buffer-restriction-once
             (mapcar #'buffer-name (persp-buffer-list-restricted nil -1 nil))
           (mapcar #'buffer-name (persp-buffer-list-restricted))))
        (indices (make-hash-table)))
    (let ((i 0))
      (dolist (elt ido-temp-list)
        (puthash elt i indices)
        (setq i (1+ i))))
    (setq ido-temp-list
          (sort buffer-names-sorted #'(lambda (a b)
                                        (< (gethash a indices 10000)
                                           (gethash b indices 10000)))))))

(defun ido-toggle-persp-filter ()
  (interactive)
  (setq persp-disable-buffer-restriction-once
        (not persp-disable-buffer-restriction-once)
        ido-text-init ido-text ido-exit 'refresh)
  (exit-minibuffer))


(defun* persp-read-buffer
    (prompt &optional default require-match predicate multiple (default-mode t))
  "Read buffers with restriction."
  (setq persp-disable-buffer-restriction-once nil)

  (when default
    (unless (stringp default)
      (if (and (bufferp default) (buffer-live-p default))
          (setq default (buffer-name default))
        (setq default nil))))

  (if prompt
      (setq prompt (car (split-string prompt ": *$" t)))
    (setq prompt "Please provide a buffer name: "))

  (let* ((buffer-names (mapcar #'buffer-name
                               (delete-if #'persp-buffer-filtered-out-p
                                          (persp-buffer-list-restricted))))
         cp retlst
         (done_str "[>done<]") (not-finished default-mode)

         (push-keys (alist-get 'push-item persp-read-multiple-keys))
         (pop-keys (alist-get 'pop-item persp-read-multiple-keys))
         push-keys-backup pop-keys-backup
         (toggle-filter-keys (alist-get 'toggle-persp-buffer-filter persp-read-multiple-keys))
         toggle-filter-keys-backup

         exit-minibuffer-function mb-local-key-map
         (persp-minibuffer-setup
          #'(lambda ()
              (setq mb-local-key-map (current-local-map))
              (when (keymapp mb-local-key-map)
                (unless exit-minibuffer-function
                  (setq exit-minibuffer-function
                        (or (lookup-key mb-local-key-map (kbd "RET"))
                            persp-read-multiple-exit-minibuffer-function)))
                (unless toggle-filter-keys-backup
                  (setq toggle-filter-keys-backup
                        (lookup-key mb-local-key-map toggle-filter-keys)))
                (define-key mb-local-key-map toggle-filter-keys
                  #'(lambda () (interactive)
                      (setq not-finished 'toggle-filter)
                      (funcall exit-minibuffer-function))))))
         (persp-multiple-minibuffer-setup
          #'(lambda ()
              (when (keymapp mb-local-key-map)
                (unless push-keys-backup
                  (setq push-keys-backup
                        (lookup-key mb-local-key-map push-keys)))
                (define-key mb-local-key-map push-keys
                  #'(lambda () (interactive)
                      (setq not-finished 'push)
                      (funcall exit-minibuffer-function)))
                (unless pop-keys-backup
                  (setq pop-keys-backup
                        (lookup-key mb-local-key-map pop-keys)))
                (define-key mb-local-key-map pop-keys
                  #'(lambda () (interactive)
                      (setq not-finished 'pop)
                      (funcall exit-minibuffer-function)))))))

    (while (member done_str buffer-names)
      (setq done_str (concat ">" done_str)))

    (unwind-protect
        (progn
          (when (and default (not (member default buffer-names)))
            (setq default nil))
          (when multiple
            (add-hook 'minibuffer-setup-hook persp-multiple-minibuffer-setup))
          (add-hook 'minibuffer-setup-hook persp-minibuffer-setup)
          (while not-finished
            (setq cp (funcall persp-interactive-completion-function
                              (concat prompt
                                      (and default (concat "(default " default ")"))
                                      (and retlst
                                           (concat "< " (mapconcat #'identity retlst " ") " >"))
                                      ": ")
                              buffer-names predicate require-match nil nil default))
            (case not-finished
              (push
               (when (and cp (member cp buffer-names))
                 (if retlst
                     (when (string= cp done_str)
                       (setq not-finished nil))
                   (push done_str buffer-names))
                 (when not-finished
                   (if (eq 'reverse multiple)
                       (setq retlst (append retlst (list cp)))
                     (push cp retlst))
                   (setq buffer-names (delete cp buffer-names)
                         default done_str)))
               (when not-finished
                 (setq not-finished default-mode)))
              (pop
               (let ((last-item (pop retlst)))
                 (unless retlst (setq buffer-names (delete done_str buffer-names)
                                      default nil))
                 (when last-item
                   (push last-item buffer-names)))
               (setq not-finished default-mode))
              (toggle-filter
               (setq persp-disable-buffer-restriction-once
                     (not persp-disable-buffer-restriction-once))
               (setq buffer-names (delete-if
                                   #'(lambda (bn) (member bn retlst))
                                   (mapcar #'buffer-name
                                           (if persp-disable-buffer-restriction-once
                                               (funcall persp-buffer-list-function)
                                             (delete-if #'persp-buffer-filtered-out-p
                                                        (persp-buffer-list-restricted))))))
               (setq not-finished default-mode))
              (t
               (when (and cp (not (string= cp done_str)) (member cp buffer-names))
                 (push cp retlst))
               (setq not-finished nil))))
          (if multiple retlst (car retlst)))
      (remove-hook 'minibuffer-setup-hook persp-multiple-minibuffer-setup)
      (remove-hook 'minibuffer-setup-hook persp-minibuffer-setup)
      (when (keymapp mb-local-key-map)
        (when multiple
          (when (lookup-key mb-local-key-map push-keys)
            (define-key mb-local-key-map push-keys push-keys-backup))
          (when (lookup-key mb-local-key-map pop-keys)
            (define-key mb-local-key-map pop-keys pop-keys-backup)))
        (when (lookup-key mb-local-key-map toggle-filter-keys)
          (define-key mb-local-key-map toggle-filter-keys toggle-filter-keys-backup)))
      (setq persp-disable-buffer-restriction-once nil))))


;; Save/Load funcs:

(defun* persp-restore-window-conf (&optional (frame (selected-frame))
                                             (persp (get-frame-persp frame))
                                             new-frame-p)
  (when (and frame (not (frame-parameter frame 'persp-ignore-wconf))
             (not (let ((old-piw (frame-parameter frame 'persp-ignore-wconf-once)))
                    (when old-piw (set-frame-parameter frame 'persp-ignore-wconf-once nil))
                    old-piw)))
    (when new-frame-p (sit-for 0.01))
    (with-selected-frame frame
      (let ((pwc (safe-persp-window-conf persp))
            (split-width-threshold 2)
            (split-height-threshold 2)
            (window-safe-min-height 1)
            (window-safe-min-width 1)
            (window-min-height 1)
            (window-min-width 1)
            (window-resize-pixelwise t)
            (gr-mode (and (boundp 'golden-ratio-mode) golden-ratio-mode)))
        (when gr-mode
          (golden-ratio-mode -1))
        (unwind-protect
            (cond
             ((functionp persp-restore-window-conf-method)
              (funcall persp-restore-window-conf-method frame persp new-frame-p))
             (t
              (if pwc
                  (progn
                    (delete-other-windows)
                    (set-window-dedicated-p nil nil)
                    (condition-case-unless-debug err
                        (funcall persp-window-state-put-function pwc frame)
                      (error (message "[persp-mode] Warning: Can not restore the window configuration, \
because of the error -- %s" err)
                             (let* ((cw (selected-window))
                                    (cwb (window-buffer cw)))
                               (unless (persp-contain-buffer-p cwb persp)
                                 (persp-set-another-buffer-for-window cwb cw persp)))))
                    (when (and new-frame-p persp-is-ibc-as-f-supported)
                      (setq initial-buffer-choice #'(lambda () persp-special-last-buffer))))
                (when persp-reset-windows-on-nil-window-conf
                  (if (functionp persp-reset-windows-on-nil-window-conf)
                      (funcall persp-reset-windows-on-nil-window-conf)
                    (delete-other-windows)
                    (set-window-dedicated-p nil nil)
                    (let* ((pbs (safe-persp-buffers persp))
                           (w (selected-window))
                           (wb (window-buffer w)))
                      (when (and pbs (not (memq wb pbs)))
                        (persp-set-another-buffer-for-window wb w persp))))))))
          (when gr-mode
            (golden-ratio-mode 1)))))))


;; Save funcs

(defun* persp-frame-save-state (&optional (frame (selected-frame)) set-persp-special-last-buffer)
  (when (and frame
             (not (persp-is-frame-daemons-frame frame))
             (not (frame-parameter frame 'persp-ignore-wconf))
             (not (frame-parameter frame 'persp-ignore-wconf-once)))
    (let ((persp (get-frame-persp frame)))
      (with-selected-frame frame
        (when set-persp-special-last-buffer
          (persp-special-last-buffer-make-current))
        (if persp
            (setf (persp-window-conf persp) (funcall persp-window-state-get-function frame))
          (setq persp-nil-wconf (funcall persp-window-state-get-function frame)))))))

(defun* persp-save-state (&optional (persp (get-frame-persp)) exfr set-persp-special-last-buffer)
  (let ((frame (selected-frame)))
    (when (eq frame exfr) (setq frame nil))
    (unless (and frame (eq persp (get-frame-persp frame)))
      (setq frame (find-other-frame-with-persp persp exfr t)))
    (when frame (persp-frame-save-state frame set-persp-special-last-buffer))))


(defun persp-buffers-to-savelist (persp)
  (delete-if #'symbolp
             (let (find-ret)
               (mapcar #'(lambda (b)
                           (setq find-ret nil)
                           (find-if #'(lambda (sl) (when sl (setq find-ret sl)))
                                    persp-save-buffer-functions
                                    :key #'(lambda (s-f) (with-current-buffer b
                                                      (funcall s-f b))))
                           find-ret)
                       (if persp
                           (persp-buffers persp)
                         (delete-if-not #'persp-buffer-free-p
                                        (funcall persp-buffer-list-function)))))))

(defun persp-window-conf-to-savelist (persp)
  `(def-wconf ,(if (or persp-use-workgroups
                       (not (version< emacs-version "24.4")))
                   (safe-persp-window-conf persp)
                 nil)))

(defun persp-elisp-object-readable-p (obj)
  (let (print-length print-level)
    (or (stringp obj)
        (not (string-match-p "#<.*?>" (prin1-to-string obj))))))

(defun persp-parameters-to-savelist (persp)
  `(def-params ,(remove-if
                 #'(lambda (param)
                     (and (not (persp-elisp-object-readable-p param))
                          (message "[persp-mode] Info: The parameter %S \
of the perspective %s can't be saved."
                                   param (safe-persp-name persp))
                          t))
                 (safe-persp-parameters persp))))

(defun persp-to-savelist (persp)
  `(def-persp ,(and persp (persp-name persp))
     ,(persp-buffers-to-savelist persp)
     ,(persp-window-conf-to-savelist persp)
     ,(persp-parameters-to-savelist persp)
     ,(safe-persp-weak persp)
     ,(safe-persp-auto persp)
     ,(safe-persp-hidden persp)))

(defun persps-to-savelist (&optional phash names-regexp)
  (mapcar #'persp-to-savelist
          (delete-if (apply-partially #'persp-parameter 'dont-save-to-file)
                     (persp-persps (or phash *persp-hash*) names-regexp t))))

(defsubst persp-save-with-backups (fname)
  (when (and (string= fname
                      (concat (expand-file-name persp-save-dir)
                              persp-auto-save-fname))
             (> persp-auto-save-num-of-backups 0))
    (do ((cur persp-auto-save-num-of-backups (1- cur))
         (prev (1- persp-auto-save-num-of-backups) (1- prev)))
        ((> 1 cur) nil)
      (let ((cf (concat fname (number-to-string cur)))
            (pf (concat fname (if (> prev 0)
                                  (number-to-string prev)
                                ""))))
        (when (file-exists-p pf)
          (when (file-exists-p cf)
            (delete-file cf))
          (rename-file pf cf t))))
    (when (file-exists-p fname)
      (rename-file fname (concat fname (number-to-string 1)) t)))
  (write-file fname nil)
  t)

(defun* persp-save-state-to-file
    (&optional (fname persp-auto-save-fname) (phash *persp-hash*)
               (respect-persp-file-parameter persp-auto-save-persps-to-their-file)
               (keep-others-in-non-parametric-file 'no))
  (interactive (list (read-file-name "Save perspectives to a file: "
                                     persp-save-dir)))
  (when (and fname phash)
    (let* ((p-save-dir (or (file-name-directory fname)
                           (expand-file-name persp-save-dir)))
           (p-save-file (concat p-save-dir (file-name-nondirectory fname))))
      (unless (and (file-exists-p p-save-dir)
                   (file-directory-p p-save-dir))
        (message "[persp-mode] Info: Trying to create the `persp-conf-dir'.")
        (make-directory p-save-dir t))
      (if (not (and (file-exists-p p-save-dir)
                    (file-directory-p p-save-dir)))
          (progn
            (message "[persp-mode] Error: Can't save perspectives -- `persp-save-dir' \
does not exists or not a directory %S." p-save-dir)
            nil)
        (mapc #'persp-save-state (persp-persps phash))
        (run-hook-with-args 'persp-before-save-state-to-file-functions
                            fname phash respect-persp-file-parameter)
        (if (and respect-persp-file-parameter
                 (member-if (apply-partially #'persp-parameter 'persp-file)
                            (persp-persps phash nil)))
            (let (persp-auto-save-persps-to-their-file
                  persp-before-save-state-to-file-functions)
              (mapc #'(lambda (gr)
                        (destructuring-bind (pfname . pl) gr
                          (let ((names (mapcar #'safe-persp-name pl)))
                            (if pfname
                                (persp-save-to-file-by-names pfname phash names 'yes nil)
                              (persp-save-to-file-by-names
                               p-save-file phash names keep-others-in-non-parametric-file nil)))))
                    (persp-group-by (apply-partially #'persp-parameter 'persp-file)
                                    (persp-persps phash nil t) t)))
          (with-temp-buffer
            (buffer-disable-undo)
            (erase-buffer)
            (goto-char (point-min))
            (insert ";; -*- mode: emacs-lisp; eval: (progn (pp-buffer) (indent-buffer)) -*-")
            (newline)
            (insert (let (print-length print-level)
                      (prin1-to-string (persps-to-savelist phash))))
            (persp-save-with-backups p-save-file)))))))

(defun* persp-save-to-file-by-names
    (&optional (fname persp-auto-save-fname) (phash *persp-hash*) names keep-others
               (called-interactively-p (called-interactively-p 'any)))
  (interactive)
  (unless names
    (setq names (persp-read-persp "to save" 'reverse (safe-persp-name (get-current-persp))
                                  t nil nil nil nil 'push)))
  (when (or (not fname) called-interactively-p)
    (setq fname (read-file-name (format "Save a subset of perspectives%s to a file: "
                                        names)
                                persp-save-dir)))
  (when names
    (unless keep-others
      (setq keep-others (if (and (file-exists-p fname)
                                 (yes-or-no-p "Keep other perspectives in the file?"))
                            'yes 'no)))
    (let ((temphash (make-hash-table :test 'equal :size 10))
          (persp-nil-wconf persp-nil-wconf)
          (persp-nil-parameters (copy-tree persp-nil-parameters))
          (persp-nil-hidden persp-nil-hidden)
          bufferlist-diff)
      (when (or (eq keep-others 'yes) (eq keep-others t))
        (let ((bufferlist-pre
               (mapcar #'(lambda (b) (cons b (persp--buffer-in-persps b)))
                       (funcall persp-buffer-list-function))))
          (persp-load-state-from-file fname temphash (cons :not (regexp-opt names)))
          (setq bufferlist-diff (delete-if #'(lambda (bcons)
                                               (destructuring-bind (buf . buf-persps) bcons
                                                 (when buf
                                                   (persp--buffer-in-persps-set buf buf-persps)
                                                   t)))
                                           (funcall persp-buffer-list-function)
                                           :key #'(lambda (b) (assq b bufferlist-pre))))))
      (mapc #'(lambda (p)
                (persp-add p temphash)
                (when (and p persp-auto-save-persps-to-their-file)
                  (set-persp-parameter 'persp-file fname p)))
            (mapcar #'(lambda (pn) (persp-get-by-name pn phash)) names))
      (persp-save-state-to-file fname temphash nil)
      (mapc #'kill-buffer bufferlist-diff))))

(defun persp-tramp-save-buffer (b)
  (let* ((buf-f-name (buffer-file-name b))
         (persp-tramp-file-name
          (when (and (or (featurep 'tramp) (require 'tramp nil t))
                     (tramp-tramp-file-p buf-f-name))
            (let ((dissected-f-name (tramp-dissect-file-name buf-f-name))
                  tmh)
              (if (tramp-file-name-hop dissected-f-name)
                  (when (and
                         (or (featurep 'tramp-sh) (require 'tramp-sh nil t))
                         (fboundp 'tramp-compute-multi-hops)
                         (setq tmh (condition-case-unless-debug err
                                       (tramp-compute-multi-hops dissected-f-name)
                                     (error nil))))
                    (let ((persp-tramp-file-name tramp-prefix-format))
                      (while tmh
                        (let* ((hop (car tmh))
                               (method   (tramp-file-name-method hop))
                               (user     (tramp-file-name-user hop))
                               (host     (tramp-file-name-host hop))
                               (filename (tramp-file-name-localname hop)))
                          (setq persp-tramp-file-name
                                (concat
                                 persp-tramp-file-name
                                 method tramp-postfix-method-format
                                 user (when user tramp-postfix-user-format)
                                 host (if (= (string-width filename) 0)
                                          tramp-postfix-hop-format
                                        (concat tramp-postfix-host-format filename)))
                                tmh (cdr tmh))))
                      persp-tramp-file-name))
                buf-f-name)))))
    (when persp-tramp-file-name
      `(def-buffer ,(buffer-name b)
         ,persp-tramp-file-name
         ,(buffer-local-value 'major-mode b)))))

;; Load funcs

(defun persp-update-frames-window-confs (&optional persp-names)
  (mapc #'persp-restore-window-conf
        (if persp-names
            (delete-if-not #'(lambda (pn) (member pn persp-names))
                           (persp-frame-list-without-daemon)
                           :key #'(lambda (f) (safe-persp-name (get-frame-persp f))))
          (persp-frame-list-without-daemon))))

(defmacro persp-car-as-fun-cdr-as-args (lst)
  (let ((kar (gensym "lst-car")))
    `(let* ((,kar (car-safe ,lst))
            (args (cdr-safe ,lst))
            (fun (or (condition-case-unless-debug err
                         (symbol-function ,kar)
                       (error nil))
                     (symbol-value ,kar))))
       (if (functionp fun)
           (apply fun args)
         (message "[persp-mode] Error: %s is not a function." fun)))))

(defun persp-buffer-from-savelist (savelist)
  (when (eq (car savelist) 'def-buffer)
    (let (persp-add-buffer-on-find-file
          (def-buffer
            #'(lambda (name fname mode &optional parameters)
                (let ((buf (persp-get-buffer-or-null name)))
                  (if buf
                      (if (or (null fname)
                              (string= fname (buffer-file-name buf)))
                          buf
                        (if (file-exists-p fname)
                            (setq buf (find-file-noselect fname))
                          (message "[persp-mode] Warning: The file %s no longer exists." fname)
                          (setq buf nil)))
                    (if (and fname (file-exists-p fname))
                        (setq buf (find-file-noselect fname))
                      (when fname
                        (message "[persp-mode] Warning: The file %s no longer exists." fname))
                      (setq buf (get-buffer-create name))))
                  (when (buffer-live-p buf)
                    (macrolet ((restorevars ()
                                `(mapc #'(lambda (varcons)
                                           (destructuring-bind (vname . vvalue) varcons
                                             (unless (or (eq vname 'buffer-file-name)
                                                         (eq vname 'major-mode))
                                               (set (make-local-variable vname) vvalue))))
                                       (alist-get 'local-vars parameters))))
                      (with-current-buffer buf
                        (restorevars)
                        (typecase mode
                          (function (when (and (not (eq major-mode mode))
                                               (not (eq major-mode 'not-loaded-yet)))
                                      (funcall mode)
                                      (restorevars)))))))
                  buf))))
      (persp-car-as-fun-cdr-as-args savelist))))

(defun persp-buffers-from-savelist-0 (savelist)
  (delete-if-not
   #'persp-get-buffer-or-null
   (let (find-ret)
     (mapcar #'(lambda (saved-buf)
                 (setq find-ret nil)
                 (find-if #'(lambda (lb) (when lb (setq find-ret lb)))
                          persp-load-buffer-functions
                          :key #'(lambda (l-f) (funcall l-f saved-buf)))
                 find-ret)
             savelist))))

(defun persp-window-conf-from-savelist-0 (savelist)
  (let ((def-wconf #'identity))
    (persp-car-as-fun-cdr-as-args savelist)))

(defun persp-parameters-from-savelist-0 (savelist)
  (let ((def-params #'identity))
    (persp-car-as-fun-cdr-as-args savelist)))

(defun persp-from-savelist-0 (savelist phash persp-file)
  (let ((def-persp
          #'(lambda (name dbufs dwc &optional dparams weak auto hidden)
              (let* ((pname (or name persp-nil-name))
                     (persp (persp-add-new pname phash)))
                (mapc #'(lambda (b)
                          (persp-add-buffer b persp nil nil))
                      (persp-buffers-from-savelist-0 dbufs))
                (if persp
                    (setf (persp-window-conf persp)
                          (persp-window-conf-from-savelist-0 dwc))
                  (setq persp-nil-wconf
                        (persp-window-conf-from-savelist-0 dwc)))
                (modify-persp-parameters (persp-parameters-from-savelist-0 dparams)
                                         persp)
                (when persp
                  (setf (persp-weak persp) weak
                        (persp-auto persp) auto))

                (if persp
                    (setf (persp-hidden persp) hidden)
                  (setq persp-nil-hidden hidden))

                (when persp-file
                  (set-persp-parameter 'persp-file persp-file persp))
                pname))))
    (persp-car-as-fun-cdr-as-args savelist)))

(defun persps-from-savelist-0 (savelist phash persp-file set-persp-file names-regexp)
  (when (and names-regexp (not (consp names-regexp)))
    (setq names-regexp (cons t names-regexp)))
  (mapcar #'(lambda (pd)
              (persp-from-savelist-0 pd phash (and set-persp-file persp-file)))
          (if names-regexp
              (delete-if-not
               (apply-partially #'persp-string-match-p names-regexp)
               savelist
               :key #'(lambda (pd) (or (cadr pd) persp-nil-name)))
            savelist)))

(defun persp-names-from-savelist-0 (savelist)
  (mapcar #'(lambda (pd) (or (cadr pd) persp-nil-name)) savelist))

(defun persps-savelist-version-string (savelist)
  (let* ((version-list (car savelist))
         (version (or (and (eq (car version-list)
                               'def-persp-save-format-version)
                           (cadr version-list))
                      0)))
    (list
     (format "%S" version)
     (if (eq version 0)
         savelist
       (cdr savelist)))))

(defun persp-dispatch-loadf-version (funsym savelist)
  (destructuring-bind (version s-list)
      (persps-savelist-version-string savelist)
    (let ((funame (intern (concat (symbol-name funsym) "-" version))))
      (if (fboundp funame)
          (list funame s-list)
        (message "[persp-mode] Warning: Can not find load function for this version: %S."
                 version)
        (list nil s-list)))))

(defun persps-from-savelist (savelist phash persp-file set-persp-file names-regexp)
  (destructuring-bind (fun s-list)
      (persp-dispatch-loadf-version 'persps-from-savelist savelist)
    (if fun
        (let ((persp-names
               (funcall fun s-list phash persp-file set-persp-file names-regexp)))
          (run-hook-with-args 'persp-after-load-state-functions persp-file phash
                              persp-names)
          persp-names)
      (message "[persp-mode] Error: Can not load perspectives from savelist: %s\n\tloaded from %s"
               savelist persp-file)
      nil)))

(defun persp-list-persp-names-in-file (fname)
  (when (and fname (file-exists-p fname))
    (let* ((pslist (with-temp-buffer
                     (buffer-disable-undo)
                     (insert-file-contents fname nil nil nil t)
                     (goto-char (point-min))
                     (read (current-buffer)))))
      (destructuring-bind (fun s-list)
          (persp-dispatch-loadf-version 'persp-names-from-savelist pslist)
        (if fun
            (funcall fun s-list)
          (message "[persp-mode] Error: Can not list perspective names in file %S."
                   fname))))))


(defun* persp-load-state-from-file (&optional (fname persp-auto-save-fname) (phash *persp-hash*)
                                              names-regexp set-persp-file)
  (interactive (list (read-file-name "Load perspectives from a file: "
                                     persp-save-dir)))
  (when fname
    (let ((p-save-file (concat (or (file-name-directory fname)
                                   (expand-file-name persp-save-dir))
                               (file-name-nondirectory fname))))
      (if (not (file-exists-p p-save-file))
          (progn (message "[persp-mode] Error: No such file -- %S." p-save-file)
                 nil)
        (let ((readed-list
               (with-temp-buffer
                 (buffer-disable-undo)
                 (insert-file-contents p-save-file nil nil nil t)
                 (goto-char (point-min))
                 (read (current-buffer)))))
          (persps-from-savelist
           readed-list phash p-save-file set-persp-file names-regexp))))))

(defun* persp-load-from-file-by-names (&optional (fname persp-auto-save-fname)
                                                 (phash *persp-hash*)
                                                 names)
  (interactive (list (read-file-name "Load a subset of perspectives from a file: "
                                     persp-save-dir)))
  (unless names
    (let* ((p-save-file (concat (or (file-name-directory fname)
                                    (expand-file-name persp-save-dir))
                                (file-name-nondirectory fname)))
           (available-names (persp-list-persp-names-in-file p-save-file)))
      (setq names (persp-read-persp "to load" 'reverse nil t nil nil available-names nil 'push))))
  (when names
    (let ((names-regexp (regexp-opt names)))
      (persp-load-state-from-file fname phash names-regexp t))))

(when persp-mode
  (mapc #'persp-find-and-set-persps-for-buffer
        (buffer-list)))

(provide 'persp-mode)

;;; persp-mode.el ends here
