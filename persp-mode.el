;;; persp-mode.el --- "perspectives" shared among frames + save/load - bugs.

;; Copyright (C) 2012 Constantin Kulikov

;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Version: 1.2.9
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

;; Based on the perspective.el by Nathan Weizenbaum
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

;; s -- create/switch to perspective.
;; r -- rename perspective.
;; c -- kill perspective
;;   (if you kill nil('none') persp -- it'll kill all opened buffers).
;; a -- add buffer to perspective.
;; t -- switch to buffer without adding it to current perspective.
;; i -- import all buffers from another perspective.
;; k -- remove buffer from perspective.
;; w -- save perspectives to file.
;; l -- load perspectives from file.
;; o -- switch off persp-mode
;;   (This may be usefull when you launch emacs just to edit a single file and don't want to
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

(unless (boundp 'iswitchb-mode)
  (setq iswitchb-mode nil))

;; Customization variables:

(unless
    (find 'custom-group (symbol-plist 'session))
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
  :type 'string)

(defface persp-face-lighter-buffer-not-in-persp
  '((t :inherit error))
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
  :type 'list)

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

(defcustom persp-auto-save-opt 2
  "This variable controls the autosave functionality of the persp-mode:
0 -- do not auto save;
1 -- save on the emacs shutdown and only if the persp-mode active;
2 -- save on the persp-mode deactivation or the emacs shutdown."
  :group 'persp-mode
  :type '(choice
          (integer :tag "Do not save"  :value 0)
          (integer :tag "Save on exit" :value 1)
          (integer :tag "Save on exit and persp-mode deactivation" :value 2)))

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
          (const :tag "Constrain to current
perspective's buffers."
                 :value t)
          (const :tag "Do not set frames'
buffer-predicate parameter."
                 :value nil)
          (const :tag "Constrain with
persp-buffer-list-restricted."
                 :value restricted-buffer-list)
          (number :tag "Constrain with
persp-buffer-list-restricted and use the value of this variable as
the restriction option (see the *persp-restrict-buffers-to* variable)."
                  :value 0)
          (function :tag "Constrain with function
which take buffer as argument."
                    :value (lambda (b) b)))
  :set #'(lambda (sym val)
           (set-default sym val)
           (if persp-mode
               (persp-update-frames-buffer-predicate)
             (add-hook 'persp-mode-hook #'persp-update-frames-buffer-predicate))))

(defvar persp-interactive-completion-function
  (cond (ido-mode      #'ido-completing-read)
        (iswitchb-mode #'persp-iswitchb-completing-read)
        (t             #'completing-read))
  "The function which is used by the persp-mode
to interactivly read user input with completion.")

(defun persp-update-completion-system (system &optional remove)
  (interactive)
  (when (and (not system) (not remove))
    (setq
     system
     (intern
      (funcall persp-interactive-completion-function
               "Set the completion system for persp-mode: "
               '("ido" "iswitchb" "completing-read")
               nil t))))

  (setq persp-interactive-completion-function #'completing-read)
  (when (boundp 'persp-interactive-completion-system)
    (case persp-interactive-completion-system
      ('ido
       (remove-hook 'ido-make-buffer-list-hook   #'persp-restrict-ido-buffers)
       (remove-hook 'ido-setup-hook              #'persp-ido-setup))
      ('iswitchb
       (remove-hook 'iswitchb-minibuffer-setup-hook #'persp-iswitchb-setup)
       (remove-hook 'iswitchb-make-buflist-hook     #'persp-iswitchb-filter-buflist)
       (remove-hook 'iswitchb-define-mode-map-hook  #'persp-iswitchb-define-mode-map))
      (t
       (setq read-buffer-function persp-saved-read-buffer-function))))

  (when system
    (set-default 'persp-interactive-completion-system system))

  (unless remove
    (case persp-interactive-completion-system
      ('ido
       (add-hook 'ido-make-buffer-list-hook   #'persp-restrict-ido-buffers)
       (add-hook 'ido-setup-hook              #'persp-ido-setup)
       (setq persp-interactive-completion-function #'ido-completing-read))
      ('iswitchb
       (add-hook 'iswitchb-minibuffer-setup-hook   #'persp-iswitchb-setup)
       (add-hook 'iswitchb-make-buflist-hook       #'persp-iswitchb-filter-buflist)
       (setq persp-interactive-completion-function #'persp-iswitchb-completing-read)
       (add-hook 'iswitchb-define-mode-map-hook    #'persp-iswitchb-define-mode-map))
      (t
       (setq persp-saved-read-buffer-function read-buffer-function)
       (setq read-buffer-function #'persp-read-buffer)))
    (persp-set-toggle-read-persp-filter-keys
     persp-toggle-read-persp-filter-keys)))

(defcustom persp-interactive-completion-system
  (cond (ido-mode      'ido)
        (iswitchb-mode 'iswitchb)
        (t             'completing-read))
  "What completion system to use."
  :group 'persp-mode
  :type '(choice
          (const :tag "ido"             :value ido)
          (const :tag "iswitchb"        :value iswitchb)
          (const :tag "completing-read" :value completing-read))
  :set #'(lambda (sym val)
           (if persp-mode
               (persp-update-completion-system val)
             (set-default sym val))))

(defcustom persp-switch-to-added-buffer t
  "If t then after you add a buffer to the current perspective
the currently selected window will be switched to that buffer."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-when-kill-switch-to-buffer-in-perspective nil
  "If t -- then after a buffer is killed the current window
will be switched to some previous buffer in the current perspective,
otherwise let  the emacs deside what to do."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-ignore-wconf-of-frames-created-to-edit-file t
  "If t -- set the persp-ignore-wconf frame parameter to t for frames
that were created by emacsclient with file arguments.
Also delete windows not showing that files
(this is because server-switch-hook runs after after-make-frames);
If function -- run that function."
  :group 'persp-mode
  :type '(choice
          (const    :tag "Ignore window configuration" :value t)
          (const    :tag "Do as usual"  :value nil)
          (function :tag "Run function" :value (lambda () nil))))

(defcustom persp-add-buffer-on-find-file t
  "If t -- add a buffer with opened file to current perspective."
  :group 'persp-mode
  :type 'boolean)

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
          (const :tag "Add if the buffer is not already in any other persp" :value free)
          (function :tag "Run this function" :value (lambda () nil)))
  :set #'(lambda (sym val)
           (set-default sym val)
           (when persp-mode
             (if val
                 (add-hook 'after-change-major-mode-hook #'persp-after-change-major-mode-h t)
               (remove-hook 'after-change-major-mode-hook #'persp-after-change-major-mode-h)))))

(defcustom persp-kill-foreign-buffer-action 'dont-ask-weak
  "What to do when manually killing a buffer that is not in the current persp:
'ask       -- ask what to do;
'kill      -- just kill;
<function> -- execute that function. This function will be executed in
  kill-buffer-query-hook, so if it will return nil the buffer will not be killed;
nil        -- do not include the current buffer to buffer list if it not in the perspective.(and just kill)"
  :group 'persp-mode
  :type '(choice
          (const    :tag "Ask what to do" :value ask)
          (const    :tag "Don't ask if a buffer belongs only to weak perspectives"
                    :value dont-ask-weak)
          (const    :tag "Just kill"      :value kill)
          (function :tag "Run function"   :value (lambda () t))
          (const    :tag "do not suggest foreign buffer to the user(kill buffer)" :value nil)))

(defcustom persp-autokill-buffer-on-remove nil
  "Kill the buffer if it removed from every(or non weak) perspecive."
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
          (function :tag "Run that function with persp as an argument"
                    :value (lambda (p) p))))

(defcustom persp-common-buffer-filter-functions
  (list #'(lambda (b) (or (string-prefix-p " " (buffer-name b))
                     (string-prefix-p "Helm" (with-current-buffer b
                                               (format-mode-line mode-name))))))
  "Common buffer filters.
The list of functions wich takes a buffer as an argument.
If one of these functions returns a non nil value the buffer considered as 'filtered out'."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

(defcustom persp-buffer-list-restricted-filter-functions nil
  "Additional filters for use inside pthe `persp-buffer-list-restricted'."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

(defcustom persp-add-buffer-on-after-change-major-mode-filter-functions nil
  "Additional filters to know which buffers we dont want to add to the current perspective
after the `after-change-major-mode-hook' is fired."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

(defcustom persp-filter-save-buffers-functions
  (list #'(lambda (b) (string-prefix-p "*" (buffer-name b))))
  "Additional filters to not save unneded buffers."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

(defcustom persp-save-buffer-functions
  (list #'(lambda (b)
            (when (persp-buffer-filtered-out-p
                   b persp-filter-save-buffers-functions)
              'skip))
        #'(lambda (b)
            (if (or (featurep 'tramp) (require 'tramp nil t))
                (when (tramp-tramp-file-p (buffer-file-name b))
                  `(def-buffer ,(buffer-name b)
                     ,(persp-tramp-save-buffer-file-name b)
                     ,(buffer-local-value 'major-mode b)))
              nil))
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
  :type '(repeat (function :tag "Function")))

(defcustom persp-load-buffer-functions
  (list #'(lambda (savelist)
            (when (eq (car savelist) 'def-buffer)
              (let ((persp-add-buffer-on-find-file nil)
                    (def-buffer
                      #'(lambda (name fname mode &optional parameters)
                          (let ((buf (persp-get-buffer-or-null name)))
                            (if (buffer-live-p buf)
                                (if (or (null fname)
                                        (string= fname (buffer-file-name buf)))
                                    buf
                                  (if (file-exists-p fname)
                                      (setq buf (find-file-noselect fname))
                                    (message "[persp-mode] Warning: The file %s is no longer exists." fname)
                                    (setq buf nil)))
                              (if (and fname (file-exists-p fname))
                                  (setq buf (find-file-noselect fname))
                                (when fname
                                  (message "[persp-mode] Warning: The file %s is no longer exists." fname))
                                (setq buf (get-buffer-create name))))
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (typecase mode
                                  (function (when (and (not (eq major-mode mode))
                                                       (not (eq major-mode 'not-loaded-yet)))
                                              (funcall mode))))))
                            buf))))
                (persp-car-as-fun-cdr-as-args savelist)))))
  "Restore a buffer from a saved structure.
If a function return nil -- follow to the next function in the list.
If a function return 'skip -- don't restore a buffer."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

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
It must accept two argument -- the created perspecive and the hash to which this perspective
will be placed, you could be interested if that hash is the `*persp-hash*' or some other."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

(defcustom persp-before-kill-functions nil
  "The list of functions that runs just before a perspective will be destroyed.
It's single argument is the perspective that will be killed."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

(defcustom persp-before-switch-functions nil
  "The list of functions that runs before actually switching to a perspective.
These functions must take two arguments -- a name of a perspective to switch
(it could be a name of an unexistent perspective or it could be the same as current)
and a frame or a window for which the switching takes place."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

(defcustom persp-activated-functions nil
  "Functions that runs after a perspective has been activated.
These functions must take one argument -- a symbol,
if it is eq 'frame -- then the perspective is activated for the current frame,
if it is eq 'window -- then the perspective is activated for the current window.
The activated perspective is available with (get-current-persp)."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-use-workgroups (and (version< emacs-version "24.4")
                                     (locate-library "workgroups.el"))
  "If t -- use the workgroups.el package for saving/restoring windows configurations."
  :group 'persp-mode
  :type 'boolean
  :set #'(lambda (sym val)
           (set-default sym val)
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
  :group 'persp-mode)

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
                      "Switch to a buffer determined from WIN's fname and bname.
Return the buffer if it was found, nil otherwise."
                      (wg-abind win (fname bname)
                                (cond ((wg-awhen (get-buffer bname) (switch-to-buffer it)))
                                      (t (switch-to-buffer wg-default-buffer) nil)))))
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

;; Global variables:

;; check if the initial-buffer-choice may be a function (emacs >= 24.4)
(defvar persp-is-ibc-as-f-supported
  (or
   (not (version< emacs-version "24.4"))
   (not
    (null
     (assoc 'function
            (cdr (getf (symbol-plist 'initial-buffer-choice) 'custom-type))))))
  "t if the `initial-buffer-choice' as a function is supported in your emacs,
otherwise nil.")

(defvar persp-minor-mode-menu nil
  "Menu for the persp-mode.")

(defvar *persp-hash* nil
  "The hash table that contain perspectives")

(defvar *persp-restrict-buffers-to* 0
  "The global variable that controls the behaviour of the `persp-buffer-list-restricted'
function (Must be used only for the local rebinding):
-1 -- show all buffers;
 0 -- restrict to current perspective's buffers;
 1 -- restrict to buffers that is not in the current perspective;
 2 -- show all buffers which are not in any _other_ perspective;
 2.5 -- same as 2, but show all buffers if the current perspective is nil;
 3 -- list only _free_ buffers, that do not belong to any perspective;
 3.5 -- same as 3, but show all buffers if the current perspecive is nil;
 function -- run that function with a frame as an argument.")

(defvar persp-restrict-buffers-to-if-foreign-buffer nil
  "Override the *persp-restrict-buffers-to* if the current buffer is not in the
current perspective. If nil -- do not override.")

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

(defvar persp-disable-buffer-restriction-once nil
  "The flag used for toggling buffer filtering during read-buffer.")

(make-variable-buffer-local
 (defvar persp-buffer-in-persps nil
   "Buffer-local list of perspective names this buffer belongs to."))


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
(define-key persp-key-map (kbd "c") #'persp-kill)
(define-key persp-key-map (kbd "a") #'persp-add-buffer)
(define-key persp-key-map (kbd "t") #'persp-temporarily-display-buffer)
(define-key persp-key-map (kbd "i") #'persp-import-buffers)
(define-key persp-key-map (kbd "k") #'persp-remove-buffer)
(define-key persp-key-map (kbd "K") #'persp-kill-buffer)
(define-key persp-key-map (kbd "w") #'persp-save-state-to-file)
(define-key persp-key-map (kbd "l") #'persp-load-state-from-file)
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
    (set-default 'persp-keymap-prefix prefix)))

(defcustom persp-keymap-prefix (kbd "C-c p")
  "The prefix for activating the persp-mode keymap."
  :group 'persp-mode
  :type 'key-sequence
  :set #'(lambda (sym val) (persp-set-keymap-prefix val)))

(defun persp-set-toggle-read-persp-filter-keys (keys)
  (interactive
   (list
    (read-key-sequence
     "Now press a key sequence to be used for toggling persp filters during the read-buffer: ")))
  (if persp-mode
      (case persp-interactive-completion-system
        ('ido
         (define-key ido-buffer-completion-map keys #'ido-toggle-persp-filter)))
    (add-hook
     'persp-mode-hook
     #'(lambda () (persp-set-toggle-read-persp-filter-keys
              persp-toggle-read-persp-filter-keys))))
  (set-default 'persp-toggle-read-persp-filter-keys keys))

(defcustom persp-toggle-read-persp-filter-keys (kbd "C-x C-p")
  "Keysequence to toggle the buffer filtering during read-buffer."
  :group 'persp-mode
  :type 'key-sequence
  :set #'(lambda (sym val)
           (persp-set-toggle-read-persp-filter-keys val)))

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
               (sure-not-killing nil))
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
               (0 (safe-persp-buffers cpersp))
               (1 (let ((ret (set-difference
                              (funcall persp-buffer-list-function frame)
                              (safe-persp-buffers cpersp))))
                    (unless (persp-contain-buffer-p curbuf cpersp)
                      (setq ret (cons curbuf (delete curbuf ret))))
                    ret))
               (2 (let ((ret (delete-if #'(lambda (b)
                                            (persp-buffer-in-other-p*
                                             b cpersp persp-dont-count-weaks-in-restricted-buffer-list))
                                        (funcall persp-buffer-list-function frame))))
                    ret))
               (3 (let ((ret (delete-if #'(lambda (b)
                                            (or
                                             (and cpersp
                                                  (persp-contain-buffer-p b cpersp))
                                             (persp-buffer-in-other-p*
                                              b cpersp persp-dont-count-weaks-in-restricted-buffer-list)))
                                        (funcall persp-buffer-list-function frame))))
                    ret)))))
        (unless (= option 0)
          (setq bl (delete-if #'(lambda (b)
                                  (persp-buffer-filtered-out-p
                                   b persp-buffer-list-restricted-filter-functions))
                              bl)))
        (when (and
               (not sure-not-killing) cpersp
               persp-kill-foreign-buffer-action
               (symbolp this-command)
               (string-match-p "^.*?kill-buffer.*?$" (symbol-name this-command))
               (not (memq curbuf bl))
               (not (persp-buffer-filtered-out-p curbuf)))
          (setq bl (cons curbuf bl)))
        bl))))

(defmacro* with-persp-buffer-list
    ((&key (buffer-list-function persp-buffer-list-function)
           (restriction *persp-restrict-buffers-to*)
           (restriction-foreign-override persp-restrict-buffers-to-if-foreign-buffer)
           (frame (selected-frame))
           (sortp nil) (cache nil))
     &rest body)
  (let ((pblf-body `(persp-buffer-list-restricted ,frame ,restriction ,restriction-foreign-override)))
    (when sortp (setq pblf-body `(sort (append ,pblf-body nil) ,sortp)))
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

(defun* set-persp-parameter (param-name value
                                        &optional (persp (get-current-persp)))
  (let* ((params (safe-persp-parameters persp))
         (old-cons (assoc param-name params)))
    (if old-cons
        (setf (cdr old-cons) value)
      (if persp
          (setf (persp-parameters persp)
                (acons param-name value params))
        (setq persp-nil-parameters
              (acons param-name value params))))))

(defun* persp-parameter (param-name &optional (persp (get-current-persp)))
  (cdr-safe (assoc param-name (safe-persp-parameters persp))))

(defun* delete-persp-parameter (param-name &optional (persp (get-current-persp)))
  (when (and (not (null param-name)) (symbolp param-name))
    (if persp
        (setf (persp-parameters persp)
              (delete (assoc param-name (persp-parameters persp))
                      (persp-parameters persp)))
      (setq persp-nil-parameters
            (delete (assoc param-name persp-nil-parameters)
                    persp-nil-parameters)))))


;; Used in mode defenition:

(defun persp-mode-start-and-remove-from-make-frame-hook (f)
  (persp-mode 1)
  (remove-hook 'after-make-frame-functions #'persp-mode-start-and-remove-from-make-frame-hook))

(defun persp-asave-on-exit ()
  (when (> persp-auto-save-opt 0)
    (persp-save-state-to-file)))

(defun persp-special-last-buffer-make-current ()
  (setq persp-special-last-buffer (current-buffer)))


;; Auto persp macro:

;;;###autoload
(defmacro* def-auto-persp
    (name
     &key buffer-name file-name mode mode-name predicate
     on-match after-match hooks dyn-env
     get-buffer-expr get-persp-expr parameters noauto)
  (unless get-persp-expr
    (setq get-persp-expr `(persp-add-new ,name)))
  (let* ((mkap-persp (gensym "persp-"))
         (body (if on-match
                   `(funcall ,on-match ,name buffer ,after-match hook hook-args)
                 `(let ((,mkap-persp ,get-persp-expr))
                    (when (and (not ,noauto) ,mkap-persp)
                      (setf (persp-auto ,mkap-persp) t))
                    (modify-persp-parameters ,parameters ,mkap-persp)
                    (persp-add-buffer buffer ,mkap-persp)
                    ,(when after-match
                       `(funcall ,after-match ,mkap-persp buffer hook hook-args))
                    ,mkap-persp))))
    (when predicate
      (setq body `(when (funcall ,predicate buffer)
                    ,body)))
    (when file-name
      (setq body `(when (string-match-p ,file-name (buffer-file-name buffer))
                    ,body)))
    (when mode
      (setq body `(when (eq ',mode major-mode )
                    ,body)))
    (when mode-name
      (setq body `(when (string-match-p ,mode-name (format-mode-line mode-name))
                    ,body)))
    (when buffer-name
      (setq body `(when (string-match-p ,buffer-name (buffer-name buffer))
                    ,body)))
    (unless get-buffer-expr (setq get-buffer-expr '(current-buffer)))
    (unless hooks
      (setq hooks (cond
                   (mode
                    (let ((h (intern (concat (symbol-name mode) "-hook"))))
                      (if (boundp h) h
                        'after-change-major-mode-hook)))
                   ((or mode-name predicate buffer-name) 'after-change-major-mode-hook)
                   (file-name 'find-file-hook)
                   (t 'after-change-major-mode-hook))))
    (unless (consp hooks) (setq hooks (list hooks)))

    `(progn
       ,@(let (ret)
           (dolist (hook hooks)
             (if (and hook (boundp hook))
                 (push
                  `(add-hook
                    ',hook
                    #'(lambda (&rest hook-args)
                        (when persp-mode
                          (let ((buffer ,get-buffer-expr)
                                (hook ',hook)
                                ,@dyn-env)
                            ,body))))
                  ret)
               (message "[persp-mode] Warning: def-auto-persp -- no such hook %s." hook)))
           ret))))


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

          (setq *persp-hash* (make-hash-table :test 'equal :size 10))

          (push '(persp . writable) window-persistent-parameters)

          (persp-add-minor-mode-menu)
          (persp-add-new persp-nil-name)

          (add-hook 'find-file-hook              #'persp-add-or-not-on-find-file)
          (add-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function)
          (add-hook 'kill-buffer-hook            #'persp-kill-buffer-h)
          (add-hook 'before-make-frame-hook      #'persp-before-make-frame)
          (add-hook 'after-make-frame-functions  #'persp-init-new-frame)
          (add-hook 'delete-frame-functions      #'persp-delete-frame)
          (add-hook 'kill-emacs-hook             #'persp-asave-on-exit)
          (add-hook 'server-switch-hook          #'persp-server-switch)
          (when persp-add-buffer-on-after-change-major-mode
            (add-hook 'after-change-major-mode-hook #'persp-after-change-major-mode-h))

          (mapc #'persp-init-frame (persp-frame-list-without-daemon))

          (when (fboundp 'tabbar-mode)
            (setq tabbar-buffer-list-function #'persp-buffer-list))

          (persp-update-completion-system
           persp-interactive-completion-system)

          (if (> persp-auto-resume-time 0)
              (run-at-time persp-auto-resume-time nil
                           #'(lambda ()
                               (remove-hook 'find-file-hook #'persp-special-last-buffer-make-current)
                               (when (> persp-auto-resume-time 0)
                                 (persp-load-state-from-file)
                                 (when (buffer-live-p persp-special-last-buffer)
                                   (switch-to-buffer persp-special-last-buffer)))))
            (remove-hook 'find-file-hook #'persp-special-last-buffer-make-current))))

    (run-hooks 'persp-mode-deactivated-hook)
    (when (> persp-auto-save-opt 1) (persp-save-state-to-file))

    (remove-hook 'find-file-hook              #'persp-add-or-not-on-find-file)
    (remove-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function)
    (remove-hook 'kill-buffer-hook            #'persp-kill-buffer-h)
    (remove-hook 'before-make-frame-hook      #'persp-before-make-frame)
    (remove-hook 'after-make-frame-functions  #'persp-init-new-frame)
    (remove-hook 'delete-frame-functions      #'persp-delete-frame)
    (remove-hook 'kill-emacs-hook             #'persp-asave-on-exit)
    (remove-hook 'server-switch-hook          #'persp-server-switch)
    (when persp-add-buffer-on-after-change-major-mode
      (remove-hook 'after-change-major-mode-hook #'persp-after-change-major-mode-h))

    (when (fboundp 'tabbar-mode)
      (setq tabbar-buffer-list-function #'tabbar-buffer-list))

    (when persp-set-frame-buffer-predicate
      (persp-update-frames-buffer-predicate t))

    (persp-update-completion-system nil t)

    (mapc #'(lambda (b)
              (with-current-buffer b
                (setq persp-buffer-in-persps nil)))
          (buffer-list))

    (setq window-persistent-parameters
          (delete* (assoc 'persp window-persistent-parameters)
                   window-persistent-parameters))

    (setq *persp-hash* nil)))


;; Hooks:

(defun persp-kill-buffer-query-function ()
  "This must be the last hook in the kill-buffer-query-hook.
Otherwise if a next function in the list returns nil -- buffer will not be killed,
but just removed from a perspective."
  (block pkbqf
    (when persp-mode
      (let* ((buffer (current-buffer))
             (persp (get-current-persp))
             (foreign-check
              (if (and persp-kill-foreign-buffer-action
                       (symbolp this-command)
                       (string-match-p "^.*?kill-buffer.*?$" (symbol-name this-command))
                       (not (persp-contain-buffer-p buffer persp))
                       (not (persp-buffer-filtered-out-p buffer)))
                  (cond
                   ((functionp persp-kill-foreign-buffer-action)
                    (funcall persp-kill-foreign-buffer-action))
                   ((null persp-kill-foreign-buffer-action) t)
                   ((eq persp-kill-foreign-buffer-action 'kill) t)
                   ((and (eq 'dont-ask-weak persp-kill-foreign-buffer-action)
                         (persp-buffer-free-p buffer t)) t)
                   (t
                    (let* ((curwin (selected-window))
                           (prompt (format "You are going to kill a buffer(%s) which is not in the current perspective. It will be removed from every perspective and then killed.\nWhat do you really want to do (k - kill/K - kill and close window/c - close window/s - switch to another buffer/q - do nothing)? "
                                           (buffer-name buffer))))
                      (macrolet
                          ((clwin (w)
                                  `(run-at-time 1 nil #'(lambda (ww) (delete-window ww)) ,w))
                           (swb (b w)
                                `(run-at-time 1 nil
                                              #'(lambda (bb ww)
                                                  (with-selected-window ww
                                                    (set-window-buffer ww (persp-get-another-buffer-for-window bb ww))))
                                              ,b ,w)))
                        (case (read-char-choice prompt '(?k ?K ?c ?s ?q ?\C-g ?\C-\[))
                          ((or ?q ?\C-g ?\C-\[) nil)
                          (?k t)
                          (?K (clwin curwin) t)
                          (?c (clwin curwin) nil)
                          (?s (swb buffer curwin) nil)
                          (t t))))))
                t)))
        (if foreign-check
            (let ((pbcontain (memq buffer (safe-persp-buffers persp))))
              (when (and persp pbcontain
                         (persp-buffer-in-other-p* buffer persp))
                (persp-remove-buffer buffer persp)
                (return-from pkbqf nil)))
          (return-from pkbqf nil))))
    t))

(defun persp-kill-buffer-h ()
  (let (persp-autokill-buffer-on-remove)
    (persp-remove-buffer (current-buffer) nil t)))

(defun persp-add-or-not-on-find-file ()
  (when persp-add-buffer-on-find-file
    (let ((no-select
           (funcall persp-backtrace-frame-function
                    0 'find-file-noselect)))
      (if no-select
          (let ((persp-switch-to-added-buffer nil))
            (persp-add-buffer (current-buffer)))
        (persp-add-buffer (current-buffer))))))

(defun persp-after-change-major-mode-h ()
  (let ((buf (current-buffer)))
    (unless (persp-buffer-filtered-out-p
             buf persp-add-buffer-on-after-change-major-mode-filter-functions)
      (case persp-add-buffer-on-after-change-major-mode
        ('nil nil)
        ('free (and (persp-buffer-free-p buf) (persp-add-buffer buf)))
        ('t (persp-add-buffer buf))))))

(defun persp-server-switch ()
  (when persp-ignore-wconf-of-frames-created-to-edit-file
    (let* ((cframe (selected-frame))
           (ccp (frame-parameter cframe 'client))
           (bl (when ccp (process-get ccp 'buffers))))
      (when bl
        (if (functionp persp-ignore-wconf-of-frames-created-to-edit-file)
            (funcall persp-ignore-wconf-of-frames-created-to-edit-file)
          (set-frame-parameter cframe 'persp-ignore-wconf t)
          (mapc #'(lambda (w)
                    (unless (memq (window-buffer w) bl)
                      (delete-window w)))
                (window-list cframe 'no-minibuf)))))))


;; Misc funcs:

(defun* persp-gen-random-name (&optional name (phash *persp-hash*))
  (unless name (setq name (number-to-string (random))))
  (macrolet ((namegen () `(format "%s:%s" name (random 9))))
    (do ((nname name (namegen)))
        ((eq :+-123emptynooo (persp-get-by-name nname phash :+-123emptynooo)) nname))))

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
  (let ((ret nil))
    (maphash #'(lambda (k p)
                 (push k ret))
             phash)
    (if reverse
        (reverse ret)
      ret)))

(defun set-window-persp (persp &optional window)
  (let ((frame (window-frame window)))
    (if (eq persp (get-frame-persp frame))
        (clear-window-persp window)
      (set-window-parameter window 'persp (safe-persp-name persp)))))
(defun window-persp-set-p (&optional window)
  (window-parameter window 'persp))
(defun get-window-persp (&optional window)
  (let ((pn (window-parameter window 'persp)))
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
  (mapcar #'caddr (cddddr persp-minor-mode-menu)))

(defun* persp-get-by-name (name &optional (phash *persp-hash*) default)
  (gethash name phash default))


(defsubst* persp-names-sorted (&optional (phash *persp-hash*))
  (sort (persp-names phash nil) #'string<))

(defun persp-group-by (keyf lst)
  (let (result)
    (mapc #'(lambda (pd)
              (let* ((key (funcall keyf pd))
                     (kv (assoc key result)))
                (if kv
                    (setcdr kv (cons pd (cdr kv)))
                  (push (list key pd) result))))
          lst)
    result))

(defun* persp-persps (&optional (phash *persp-hash*) &optional names-regexp)
  (let (ret)
    (maphash #'(lambda (k p)
                 (if names-regexp
                     (when (string-match-p names-regexp k)
                       (push p ret))
                   (push p ret)))
             phash)
    ret))

(defun* persp-other-not-hidden-persps (&optional persp (phash *persp-hash*))
  (delete-if #'safe-persp-hidden (delq persp (persp-persps phash))))

(defun* persp-other-persps-with-buffer-except-nil
    (buff-or-name
     &optional persp (phash *persp-hash*) del-weak)
  (let ((buf (persp-get-buffer-or-null buff-or-name))
        ret)
    (when buf
      (setq ret (delete-if-not
                 #'(lambda (p) (when p
                            (memq buf (persp-buffers p))))
                 (delq persp (delq nil (persp-persps phash)))))
      (when del-weak
        (setq ret (delete-if #'persp-weak ret))))
    ret))
(defun persp-other-persps-with-buffer-except-nil* (buff-or-name &optional persp del-weak)
  (with-current-buffer buff-or-name
    (let ((persps persp-buffer-in-persps))
      (when persp
        (setq persps (remove (persp-name persp) persps)))
      (when del-weak
        (setq persps (remove-if
                      #'(lambda (pn) (safe-persp-weak (persp-get-by-name pn)))
                      persps)))
      persps)))

(defun* persp-buffer-in-other-p
    (buff-or-name
     &optional (persp (get-current-persp)) (phash *persp-hash*) del-weak)
  (persp-other-persps-with-buffer-except-nil buff-or-name persp phash del-weak))
(defun* persp-buffer-in-other-p* (buff-or-name &optional (persp (get-current-persp)) del-weak)
  (persp-other-persps-with-buffer-except-nil* buff-or-name persp del-weak))


(defun* persp-frames-with-persp (&optional (persp (get-frame-persp)))
  (delete-if-not #'(lambda (f)
                     (eq persp (get-frame-persp f)))
                 (persp-frame-list-without-daemon)))
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
        (let (reslist)
          (mapc #'(lambda (b)
                    (when (string-match-p regexp (buffer-name b))
                      (push (buffer-name b) reslist)))
                blist)
          (when (and reslist
                     (or noask (y-or-n-p (format "Do %s on these buffers:\n%s?\n"
                                                 func
                                                 (mapconcat 'identity reslist "\n")))))
            (mapc #'(lambda (b) (apply func b rest-args)) reslist)))))))


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
    (setq name (persp-prompt nil "to remove"
                             (and (eq phash *persp-hash*) (safe-persp-name (get-current-persp)))
                             t t)))
  (let ((persp (persp-get-by-name name phash :+-123emptynooo))
        (persp-to-switch persp-nil-name))
    (unless (eq persp :+-123emptynooo)
      (persp-save-state persp)
      (if (and (eq phash *persp-hash*) (null persp))
          (message "[persp-mode] Error: Can't remove the 'nil' perspective")
        (remhash name phash)
        (when (eq phash *persp-hash*)
          (persp-remove-from-menu persp)
          (let* ((frames-windows (persp-frames-and-windows-with-persp persp))
                 (frames (car frames-windows))
                 (windows (cdr frames-windows)))
            (dolist (w windows) (clear-window-persp w))
            ;;(setq persp-to-switch (or (car (persp-names phash nil)) persp-nil-name))
            (dolist (f frames)
              (persp-frame-switch persp-to-switch f))))))
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
    (message "[persp-mode] Error: Can't create or switch to a perspective \
with empty name.")
    nil))

(defun* persp-contain-buffer-p (buff-or-name
                                &optional (persp (get-current-persp)))
  (find (persp-get-buffer-or-null buff-or-name) (safe-persp-buffers persp)))

(defun* persp-add-buffer (buff-or-name
                          &optional (persp (get-current-persp))
                          (switchorno persp-switch-to-added-buffer))
  (interactive
   (list (let ((*persp-restrict-buffers-to* 1)
               (persp-restrict-buffers-to-if-foreign-buffer nil))
           (read-buffer "Add a buffer to the perspective: " (current-buffer) t))))
  (let ((buffer (persp-get-buffer-or-null buff-or-name)))
    (when (and persp (buffer-live-p buffer)
               (null (persp-contain-buffer-p buffer persp)))
      (push buffer (persp-buffers persp))
      (with-current-buffer buffer
        (push (persp-name persp) persp-buffer-in-persps)))
    (when (and buffer switchorno)
      (switch-to-buffer buffer))
    buffer))

(defun* persp-add-buffers-by-regexp (&optional regexp (persp (get-current-persp)))
  (interactive)
  (when persp
    (persp-do-buffer-list-by-regexp
     :regexp regexp :func 'persp-add-buffer :rest-args (list persp nil)
     :blist (persp-buffer-list-restricted (selected-frame) 1))))

(defun* persp-temporarily-display-buffer (buff-or-name)
  (interactive (list
                (let ((*persp-restrict-buffers-to* 1)
                      (persp-restrict-buffers-to-if-foreign-buffer nil)
                      (persp-temporarily-display-buffer t))
                  (read-buffer "Temporarily display a buffer, not adding it to the current perspective: "
                               nil t))))
  (let ((buffer (persp-get-buffer-or-null buff-or-name))
        (persp-temporarily-display-buffer t))
    (when buffer
      (switch-to-buffer buffer t))))

(defun* persp-remove-buffer (buff-or-name
                             &optional (persp (get-current-persp)) noask-to-remall noswitch)
  "Remove a buffer from a perspective. Switch all windows displaying that buffer
to another one. If `PERSP' is nil -- remove the buffer from all perspectives.
Return the removed buffer."
  (interactive
   (list
    (let ((*persp-restrict-buffers-to* 0)
          (persp-restrict-buffers-to-if-foreign-buffer nil))
      (read-buffer "Remove a buffer from the perspective: " (current-buffer) t))))
  (let ((buffer (persp-get-buffer-or-null buff-or-name)))
    (if (null persp)
        (when (or noask-to-remall
                  (yes-or-no-p "Remove buffer from all perspectives?"))
          (mapc #'(lambda (p)
                    (persp-remove-buffer buffer p))
                (persp-other-persps-with-buffer-except-nil buffer persp)))
      (if (memq buffer (persp-buffers persp))
          (progn
            (setf (persp-buffers persp) (delq buffer (persp-buffers persp)))
            (with-current-buffer buffer
              (setq persp-buffer-in-persps (delete (persp-name persp) persp-buffer-in-persps)))
            (if noswitch
                buffer
              (persp-switchto-prev-buf buffer persp)))))
    (when (and persp-autokill-buffer-on-remove
               (persp-buffer-free-p
                buffer (eq 'kill-weak persp-autokill-buffer-on-remove)))
      (let (persp-autokill-buffer-on-remove
            persp-kill-foreign-buffer-action)
        (kill-buffer buffer)))
    (when (and persp-autokill-persp-when-removed-last-buffer
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
        (persp-kill (safe-persp-name persp)))))
    buffer))

(defun persp-kill-buffer (&optional buf-or-name)
  "Kill buffer, take the restriction into account."
  (interactive)
  (unless buf-or-name
    (let ((*persp-restrict-buffers-to* 0)
          (persp-restrict-buffers-to-if-foreign-buffer nil))
      (setq buf-or-name
            (read-buffer "Kill buffer: " (current-buffer) t))))
  (when (and buf-or-name
             (buffer-live-p (get-buffer buf-or-name)))
    (kill-buffer buf-or-name)))

(defun* persp-remove-buffers-by-regexp (&optional regexp (persp (get-current-persp)))
  (interactive)
  (when persp
    (persp-do-buffer-list-by-regexp :regexp regexp :func 'persp-remove-buffer
                                    :blist (persp-buffers persp) :rest-args (list persp))))

(defun* persp-import-buffers
    (name
     &optional (persp-to (get-current-persp)) (phash *persp-hash*))
  "Import buffers from the perspective with the given name to another one.
If run interactively assume import from some perspective that is in the `*persp-hash*'
into the current."
  (interactive "i")
  (unless name
    (setq name (persp-prompt nil "to import buffers from" nil t nil t)))
  (let ((persp-from (persp-get-by-name name phash)))
    (persp-import-buffers-from persp-from persp-to)))

(defun* persp-import-buffers-from (persp-from
                                   &optional (persp-to (get-current-persp)))
  (if persp-to
      (mapc #'(lambda (b) (persp-add-buffer b persp-to))
            (safe-persp-buffers persp-from))
    (message "[persp-mode] Error: Can't import buffers to the 'nil' perspective, cause it already contain all buffers.")))


(defun* persp-get-buffer (buff-or-name
                          &optional (persp (get-current-persp)))
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
  (setq filters (append
                 persp-common-buffer-filter-functions
                 filters))
  (block pbfop
    (let ((buf (get-buffer buff-or-name))
          filter f)
      (while (setq filter (pop filters))
        (when
            (if (functionp filter)
                (funcall filter buf)
              (while (setq f (pop filter))
                (when (funcall f buf)
                  (return-from pbfop t))))
          (return-from pbfop t)))
      nil)))

(defun persp-buffer-free-p (buff-or-name &optional del-weak)
  (with-current-buffer buff-or-name
    (if persp-buffer-in-persps
        (if del-weak
            (not
             (find-if-not #'(lambda (pn)
                              (safe-persp-weak (persp-get-by-name pn)))
                          persp-buffer-in-persps))
          nil)
      t)))


(defun* persp-get-another-buffer-for-window
    (old-buff-or-name window
                      &optional
                      (persp (get-current-persp nil window)))
  (let* ((old-buf (persp-get-buffer-or-null old-buff-or-name))
         (p-bs (safe-persp-buffers persp))
         (buffers (delete-if #'(lambda (bc)
                                 (or
                                  (eq (car bc) old-buf)
                                  (not (find (car bc) p-bs))))
                             (append (window-prev-buffers window)
                                     (window-next-buffers window)))))
    (or (persp-get-buffer (and buffers (car (first buffers))) persp)
        (car (persp-buffer-list-restricted (window-frame window) 2.5))
        (car (buffer-list)))))

(defun* persp-switchto-prev-buf (old-buff-or-name
                                 &optional (persp (get-current-persp)))
  "Switch all windows in all frames with a perspective displaying that buffer
to some previous buffer in the perspective.
Return that old buffer."
  (let ((old-buf (persp-get-buffer-or-null old-buff-or-name)))
    (when persp-when-kill-switch-to-buffer-in-perspective
      (let* ((frames-windows (persp-frames-and-windows-with-persp persp))
             (frames (car frames-windows))
             (windows (cdr frames-windows)))
        (dolist (w windows)
          (set-window-buffer
           w
           (persp-get-another-buffer-for-window old-buf w)))
        (dolist (f frames)
          (dolist (w (get-buffer-window-list old-buf 'no-minibuf f))
            (set-window-buffer
             w
             (persp-get-another-buffer-for-window old-buf w))))))
    old-buf))

(defsubst* persp-filter-out-bad-buffers (&optional (persp (get-current-persp)))
  ;; filter out killed buffers
  (when persp
    (delete-if-not #'buffer-live-p (persp-buffers persp))))

(defun persp-hide (name)
  (interactive "i")
  (unless name
    (setq name (persp-prompt nil "to hide" (safe-persp-name (get-current-persp)) t)))
  (let* ((persp (persp-get-by-name name *persp-hash* :+-123emptynooo))
         (persp-to-switch (get-current-persp)))
    (unless (eq persp :+-123emptynooo)
      (when (eq persp persp-to-switch)
        (setq persp-to-switch (car (persp-other-not-hidden-persps persp))))
      (if persp
          (setf (persp-hidden persp) t)
        (setq persp-nil-hidden t))
      (let* ((frames-windows (persp-frames-and-windows-with-persp persp))
             (frames (car frames-windows))
             (windows (cdr frames-windows)))
        (dolist (w windows) (clear-window-persp w))
        (dolist (f frames)
          (persp-frame-switch (safe-persp-name persp-to-switch) f))))))

(defun persp-unhide (name)
  (interactive "i")
  (unless name
    (let ((hidden-persps
           (mapcar #'safe-persp-name
                   (delete-if-not #'safe-persp-hidden
                                  (persp-persps)))))
      (setq name
            (persp-prompt
             nil "to unhide" (car hidden-persps) t nil nil hidden-persps t))))
  (when name
    (let ((persp (persp-get-by-name name *persp-hash* :+-123emptynooo)))
      (unless (eq persp :+-123emptynooo)
        (if persp
            (setf (persp-hidden persp) nil)
          (setq persp-nil-hidden nil))))))

(defun persp-kill (name &optional dont-kill-buffers)
  (interactive "i")
  (unless name
    (setq name (persp-prompt nil (concat "to kill"
                                         (and dont-kill-buffers
                                              "(not killing buffers)"))
                             (safe-persp-name (get-current-persp)) t)))
  (when (or (not (string= name persp-nil-name))
            (yes-or-no-p "Really kill the 'nil' perspective (It'l kill all buffers)?"))
    (let ((persp (persp-get-by-name name *persp-hash* :+-123emptynooo))
          (cpersp (get-current-persp)))
      (unless (eq persp :+-123emptynooo)
        (run-hook-with-args 'persp-before-kill-functions persp)
        (unless dont-kill-buffers
          (let (persp-autokill-persp-when-removed-last-buffer)
            (mapc #'kill-buffer (safe-persp-buffers persp))))
        (persp-remove-by-name name)))))

(defun persp-kill-without-buffers (name)
  (interactive)
  (persp-kill name t))

(defun* persp-rename (newname
                      &optional (persp (get-current-persp)) (phash *persp-hash*))
  (interactive "sNew name: ")
  (let ((opersp (gethash newname phash))
        (old-name (safe-persp-name persp)))
    (if (and (not opersp) newname)
        (progn
          (persp-remove-from-menu persp)
          (remhash old-name phash)
          (if persp
              (progn
                (setf (persp-name persp) newname)
                (mapc #'(lambda (b)
                          (with-current-buffer b
                            (setq persp-buffer-in-persps
                                  (cons newname
                                        (delete* old-name persp-buffer-in-persps
                                                 :test #'string=)))))
                      (persp-buffers persp)))
            (message "[persp-mode] Info: You can't rename the `nil' perspective, use \
M-x: customize-variable RET persp-nil-name RET"))
          (puthash newname persp phash)
          (persp-add-to-menu persp))
      (message "[persp-mode] Error: There is already a perspective with \
that name: %s." newname)
      nil)))

(defun* persp-switch (name &optional frame (window (selected-window)))
  "Switch to the perspective with name `NAME'.
If there is no perspective with that name it will be created.
Return `NAME'."
  (interactive "i")
  (unless name
    (setq name (persp-prompt nil "to switch" nil nil nil t)))
  (unless frame (setq frame (window-frame window)))
  (if (window-persp-set-p window)
      (persp-window-switch name window)
    (persp-frame-switch name frame)))
(defun* persp-frame-switch (name &optional (frame (selected-frame)))
  (interactive "i")
  (unless name
    (setq name (persp-prompt nil "to switch" nil nil nil t)))
  (run-hook-with-args 'persp-before-switch-functions name frame)
  (if (string= name (safe-persp-name (get-frame-persp frame)))
      name
    (let ((persp (or (gethash name *persp-hash*)
                     (persp-add-new name))))
      (persp-frame-save-state frame)
      (persp-activate persp frame)))
  name)
(defun* persp-window-switch (name &optional (window (selected-window)))
  (interactive "i")
  (unless name
    (setq name (persp-prompt nil "to switch for this window" nil nil nil t)))
  (run-hook-with-args 'persp-before-switch-functions name window)
  (if (and (window-persp-set-p window)
           (string= name (safe-persp-name (get-window-persp window))))
      name
    (let ((persp (or (gethash name *persp-hash*)
                     (persp-add-new name))))
      (persp-activate persp window))))

(defun persp-before-make-frame ()
  (let ((persp (gethash (or (and persp-set-last-persp-for-new-frames
                                 persp-last-persp-name)
                            persp-nil-name) *persp-hash* :+-123emptynooo)))
    (when (eq persp :+-123emptynooo)
      (setq persp (persp-add-new persp-nil-name)))
    (persp-save-state persp nil t)))

(defun* persp-activate (persp
                        &optional (frame-or-window (selected-frame)) new-frame)
  (when frame-or-window
    (setq persp-last-persp-name (safe-persp-name persp))
    (let ((old-persp))
      (typecase frame-or-window
        (frame
         (setq old-persp (get-frame-persp frame-or-window))
         (set-frame-persp persp frame-or-window)
         (persp-restore-window-conf frame-or-window persp new-frame)
         (with-selected-frame frame-or-window
           (run-hook-with-args 'persp-activated-functions 'frame)))
        (window
         (setq old-persp (get-window-persp frame-or-window))
         (set-window-persp persp frame-or-window)
         (let ((cbuf (window-buffer frame-or-window)))
           (unless (persp-contain-buffer-p cbuf persp)
             (set-window-buffer
              frame-or-window
              (persp-get-another-buffer-for-window
               cbuf frame-or-window persp))))
         (with-selected-window frame-or-window
           (run-hook-with-args 'persp-activated-functions 'window))))
      (when (and old-persp (not (eq old-persp persp))
                 (persp-auto old-persp)
                 (null (persp-buffers old-persp))
                 persp-autokill-persp-when-removed-last-buffer)
        (cond
         ((functionp persp-autokill-persp-when-removed-last-buffer)
          (funcall persp-autokill-persp-when-removed-last-buffer old-persp))
         ((or
           (eq 'hide persp-autokill-persp-when-removed-last-buffer)
           (and (eq 'hide-auto persp-autokill-persp-when-removed-last-buffer)
                (persp-auto old-persp)))
          (persp-hide (persp-name old-persp)))
         ((or
           (eq t persp-autokill-persp-when-removed-last-buffer)
           (eq 'kill persp-autokill-persp-when-removed-last-buffer)
           (and
            (eq 'kill-auto persp-autokill-persp-when-removed-last-buffer)
            (persp-auto old-persp)))
          (persp-kill (persp-name old-persp))))))))

(defun persp-init-new-frame (frame)
  (persp-init-frame frame t))
(defun* persp-init-frame (frame &optional new-frame)
  (let ((persp (gethash (or (and persp-set-last-persp-for-new-frames
                                 persp-last-persp-name)
                            persp-nil-name) *persp-hash* :+-123emptynooo)))
    (modify-frame-parameters frame `((persp . nil)))
    (when persp-set-frame-buffer-predicate
      (persp-set-frame-buffer-predicate frame))
    (when (eq persp :+-123emptynooo)
      (setq persp (persp-add-new persp-nil-name)))
    (persp-activate persp frame new-frame)))

(defun persp-delete-frame (frame)
  (unless (or (frame-parameter frame 'persp-ignore-wconf)
              (frame-parameter frame 'persp-ignore-wconf-once))
    (let ((persp (get-frame-persp frame)))
      (persp-frame-save-state frame
                              (if persp-set-last-persp-for-new-frames
                                  (string= (safe-persp-name persp) persp-last-persp-name)
                                (null persp))))))

(defun* find-other-frame-with-persp (&optional (persp (get-frame-persp))
                                               (exframe (selected-frame))
                                               for-save)
  (let* ((flist (delq exframe (persp-frames-with-persp persp))))
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

(defun persp-prompt
    (multiple action
              &optional default require-match delnil delcur persp-list show-hidden)
  (let ((persps (or persp-list
                    (persp-names-current-frame-fast-ordered))))
    (when delnil
      (setq persps (delete persp-nil-name persps)))
    (when delcur
      (setq persps (delete (safe-persp-name (get-current-persp)) persps)))
    (unless show-hidden
      (setq persps (delete-if #'(lambda (pn)
                                  (safe-persp-hidden
                                   (persp-get-by-name pn)))
                              persps)))
    (let (retlst)
      (macrolet ((call-pif ()
                           `(funcall persp-interactive-completion-function
                                     (concat
                                      (when retlst
                                        (concat "(" (mapconcat #'identity retlst " ") ") "))
                                      "Perspective name " action
                                      (if default (concat " (default " default ")") "")
                                      ": ")
                                     persps nil require-match nil nil default)))
        (if multiple
            (let ((done_str "[>done<]")
                  cp)
              (while (member done_str persps)
                (setq done_str (concat ">" done_str)))
              (push done_str persps)
              (block 'multi-ret
                (while (setq cp (call-pif))
                  (when default (setq default nil))
                  (if (string= cp done_str)
                      (return-from 'multi-ret retlst)
                    (setq persps (delete cp persps))
                    (push cp retlst)))))
          (call-pif))))))

(defun persp-generate-frame-buffer-predicate (opt)
  (eval
   (if opt
       `(function
         (lambda (b)
           (if (string-prefix-p " " (buffer-name (current-buffer)))
               t
             ,(typecase opt
                (function
                 `(funcall ,opt b))
                (number
                 `(let ((*persp-restrict-buffers-to* ,opt))
                    (memq b (persp-buffer-list-restricted
                             (selected-frame) ,opt
                             persp-restrict-buffers-to-if-foreign-buffer t))))
                (symbol
                 (case opt
                   ('nil t)
                   ('restricted-buffer-list
                    '(memq b (persp-buffer-list-restricted
                              (selected-frame)
                              *persp-restrict-buffers-to*
                              persp-restrict-buffers-to-if-foreign-buffer
                              t)))
                   (t '(memq b (safe-persp-buffers (get-current-persp))))))
                (t t)))))
     nil)))

(defun persp-set-frame-buffer-predicate (frame &optional off)
  (lexical-let ((old-pred (frame-parameter frame 'buffer-predicate-old)))
    (let (new-pred)
      (if off
          (progn
            (when (eq :null old-pred) (setq old-pred nil))
            (set-frame-parameter frame 'buffer-predicate-old nil)
            (setq new-pred old-pred))
        (setq new-pred
              (case old-pred
                ('nil
                 (set-frame-parameter frame 'buffer-predicate-old
                                      (or (frame-parameter frame 'buffer-predicate)
                                          :null))
                 persp-frame-buffer-predicate)
                (:null
                 persp-frame-buffer-predicate)
                (t
                 (if persp-frame-buffer-predicate
                     #'(lambda (b)
                         (and
                          (funcall persp-frame-buffer-predicate b)
                          (funcall old-pred b)))
                   old-pred)))))
      (set-frame-parameter frame 'buffer-predicate new-pred))))

(defun persp-update-frames-buffer-predicate (&optional off)
  (setq persp-frame-buffer-predicate
        (persp-generate-frame-buffer-predicate
         persp-set-frame-buffer-predicate))
  (mapc #'(lambda (f) (persp-set-frame-buffer-predicate f off))
        (persp-frame-list-without-daemon)))

(defun persp-iswitchb-completing-read
    (prompt choices
            &optional predicate require-match
            initial-input hist def inherit-input-method)
  "Support for the `iswitchb-mode'."
  (let ((iswitchb-make-buflist-hook
         #'(lambda () (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt def require-match initial-input nil)))

(defun persp-iswitchb-setup ()
  (setq persp-disable-buffer-restriction-once nil))

(defun persp-iswitchb-define-mode-map ()
  (define-key
    iswitchb-mode-map
    persp-toggle-read-persp-filter-keys
    #'iswitchb-toggle-persp-filter))

(defun persp-iswitchb-filter-buflist ()
  "Support for the `iswitchb-mode'."
  (setq iswitchb-temp-buflist
        (if persp-disable-buffer-restriction-once
            (persp-buffer-list-restricted nil -1 nil)
          (persp-buffer-list-restricted))))

(defun iswitchb-toggle-persp-filter ()
  (interactive)
  (setq persp-disable-buffer-restriction-once
        (not persp-disable-buffer-restriction-once))
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))


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


(defun persp-read-buffer (prompt &optional def require-match)
  "Support for the standard read-buffer."
  (setq persp-disable-buffer-restriction-once nil)
  (let ((persp-read-buffer-reread 'reread)
        ret)
    (while persp-read-buffer-reread
      (setq persp-read-buffer-reread nil)
      (let* ((read-buffer-function nil)
             (rb-completion-table (persp-complete-buffer))
             (persp-minibuffer-setup
              #'(lambda ()
                  (setq minibuffer-completion-table
                        rb-completion-table)
                  (define-key minibuffer-local-map
                    persp-toggle-read-persp-filter-keys
                    #'persp-read-buffer-toggle-persp-filter)))
             (persp-minibuffer-exit
              #'(lambda ()
                  (unless persp-read-buffer-reread
                    (define-key minibuffer-local-map
                      persp-toggle-read-persp-filter-keys nil)))))
        (unwind-protect
            (progn
              (add-hook 'minibuffer-setup-hook persp-minibuffer-setup t)
              (add-hook 'minibuffer-exit-hook persp-minibuffer-exit t)
              (setq ret (read-buffer prompt def require-match)))
          (remove-hook 'minibuffer-setup-hook persp-minibuffer-setup)
          (remove-hook 'minibuffer-exit-hook persp-minibuffer-exit))))
    ret))

(defun persp-read-buffer-toggle-persp-filter ()
  (interactive)
  (setq persp-disable-buffer-restriction-once
        (not persp-disable-buffer-restriction-once)
        persp-read-buffer-reread t)
  (exit-minibuffer))

(defun persp-complete-buffer ()
  "Complete buffer."
  (lexical-let ((buffer-names-sorted
                 (mapcar #'buffer-name
                         (if persp-disable-buffer-restriction-once
                             (persp-buffer-list-restricted nil -1 nil)
                           (persp-buffer-list-restricted)))))
    (apply-partially #'completion-table-with-predicate
                     (or minibuffer-completion-table 'internal-complete-buffer)
                     #'(lambda (name)
                         (member (if (consp name) (car name) name)
                                 buffer-names-sorted ))
                     nil)))


;; Save/Load funcs:

(defun* persp-restore-window-conf (&optional (frame (selected-frame))
                                             (persp (get-frame-persp frame))
                                             new-frame)
  (when (and frame (not (frame-parameter frame 'persp-ignore-wconf))
             (not (let ((old-piw (frame-parameter frame 'persp-ignore-wconf-once)))
                    (when old-piw (set-frame-parameter frame 'persp-ignore-wconf-once nil))
                    old-piw)))
    (when new-frame (sit-for 0.01))
    (with-selected-frame frame
      (let ((pwc (safe-persp-window-conf persp))
            (split-width-threshold 0)
            (split-height-threshold 0)
            (window-min-height window-safe-min-height)
            (window-min-width window-safe-min-width)
            (gr-mode (and (boundp 'golden-ratio-mode) golden-ratio-mode)))
        (when gr-mode
          (golden-ratio-mode -1))
        (unwind-protect
            (cond
             ((functionp persp-restore-window-conf-method)
              (funcall persp-restore-window-conf-method frame persp new-frame))
             (t
              (if pwc
                  (progn
                    (delete-other-windows)
                    (set-window-dedicated-p nil nil)
                    (condition-case err
                        (funcall persp-window-state-put-function pwc frame)
                      (error (message "[persp-mode] Warning: Can not restore the window configuration, because of the error -- %s" err)))
                    (when (and new-frame persp-is-ibc-as-f-supported)
                      (setq initial-buffer-choice #'(lambda () persp-special-last-buffer))))
                (when persp-reset-windows-on-nil-window-conf
                  (if (functionp persp-reset-windows-on-nil-window-conf)
                      (funcall persp-reset-windows-on-nil-window-conf)
                    (delete-other-windows)
                    (set-window-dedicated-p nil nil))))))
          (when gr-mode
            (golden-ratio-mode 1)))))))


(defun* persp-frame-save-state (&optional (frame (selected-frame)) set-persp-special-last-buffer)
  (let ((persp (get-frame-persp frame)))
    (when (and frame
               (not (persp-is-frame-daemons-frame frame))
               (not (frame-parameter frame 'persp-ignore-wconf))
               (not (frame-parameter frame 'persp-ignore-wconf-once)))
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


(defsubst persp-save-all-persps-state ()
  (mapc #'persp-save-state (persp-persps)))


;; Save funcs

(defun persp-buffers-to-savelist (persp)
  (let (ret)
    (mapc #'(lambda (b)
              (block 'persp-buffer-to-savelist
                (let (tmp)
                  (dolist (s-f persp-save-buffer-functions)
                    (setq tmp (funcall s-f b))
                    (when tmp
                      (when (eq tmp 'skip) (return-from 'persp-buffer-to-savelist))
                      (push tmp ret)
                      (return-from 'persp-buffer-to-savelist))))))
          (safe-persp-buffers persp))
    ret))

(defun persp-window-conf-to-savelist (persp)
  `(def-wconf ,(if (or persp-use-workgroups
                       (not (version< emacs-version "24.4")))
                   (safe-persp-window-conf persp)
                 nil)))

(defun persp-parameters-to-savelist (persp)
  `(def-params ,(remove-if
                 #'(lambda (param)
                     (and (not (stringp param))
                          (string-match-p "#<.*?>"
                                          (prin1-to-string param))
                          (message "[persp-mode] Info: The parameter %S \
of the perspective %s can't be saved."
                                   param (safe-persp-name persp))
                          t))
                 (safe-persp-parameters persp))))

(defun persp-to-savelist (persp)
  `(def-persp ,(and persp (safe-persp-name persp))
     ,(persp-buffers-to-savelist persp)
     ,(persp-window-conf-to-savelist persp)
     ,(persp-parameters-to-savelist persp)
     ,(safe-persp-weak persp)
     ,(safe-persp-auto persp)
     ,(safe-persp-hidden persp)))

(defun persps-to-savelist (phash &optional names-regexp)
  (mapcar #'persp-to-savelist
          (delete-if #'(lambda (p)
                         (persp-parameter 'dont-save-to-file p))
                     (persp-persps phash names-regexp))))

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
  (write-file fname nil))

(defun* persp-save-state-to-file (&optional (fname persp-auto-save-fname)
                                            (phash *persp-hash*)
                                            (respect-persp-file-parameter persp-auto-save-persps-to-their-file))
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
          (message "[persp-mode] Error: Can't save perspectives -- `persp-save-dir' \
does not exists or not a directory %S." p-save-dir)
        (persp-save-all-persps-state)
        (if respect-persp-file-parameter
            (let ((fg (persp-group-by #'(lambda (p) (persp-parameter 'persp-file p))
                                      (persp-persps phash)))
                  (persp-auto-save-persps-to-their-file nil))
              (mapc #'(lambda (gr)
                        (let ((pfname (car gr)) (pl (cdr gr)) names)
                          (mapc #'(lambda (p) (push (safe-persp-name p) names)) pl)
                          (if pfname
                              (persp-save-to-file-by-names pfname phash names 'yes)
                            (persp-save-to-file-by-names p-save-file phash names 'no))))
                    fg))
          (with-temp-buffer
            (erase-buffer)
            (goto-char (point-min))
            (insert (let ((print-length nil)
                          (print-level nil))
                      (prin1-to-string (persps-to-savelist phash))))
            (persp-save-with-backups p-save-file)))))))

(defun* persp-save-to-file-by-names (&optional (fname persp-auto-save-fname)
                                               (phash *persp-hash*)
                                               names keep-others)
  (interactive)
  (unless names
    (setq names (persp-prompt t "to save" (safe-persp-name (get-current-persp)) t)))
  (when (or (not fname) (called-interactively-p 'any))
    (setq fname (read-file-name (format "Save a subset of perspectives%s to a file: "
                                        names)
                                persp-save-dir)))
  (when names
    (unless keep-others
      (setq keep-others (if (and (file-exists-p fname) (yes-or-no-p "Keep other perspectives in the file?"))
                            'yes 'no)))
    (let ((temphash (make-hash-table :test 'equal :size 10))
          (persp-nil-wconf persp-nil-wconf)
          (persp-nil-parameters (copy-tree persp-nil-parameters))
          (persp-nil-hidden persp-nil-hidden)
          bufferlist-pre bufferlist-diff)
      (when (or (eq keep-others 'yes) (eq keep-others t))
        (setq bufferlist-pre (funcall persp-buffer-list-function))
        (persp-load-state-from-file fname temphash (concat "[^" (regexp-opt names) "]"))
        (setq bufferlist-diff (delete-if #'(lambda (b) (memq b bufferlist-pre))
                                         (funcall persp-buffer-list-function))))
      (mapc #'(lambda (pn)
                (let ((p (persp-add (persp-get-by-name pn phash) temphash)))
                  (when (and p persp-auto-save-persps-to-their-file)
                    (set-persp-parameter 'persp-file fname p))))
            names)
      (persp-save-state-to-file fname temphash nil)
      (mapc #'kill-buffer bufferlist-diff))))

(defun persp-tramp-save-buffer-file-name (b)
  (let ((persp-tramp-file-name tramp-prefix-format)
        (tmh (tramp-compute-multi-hops (tramp-dissect-file-name (buffer-file-name b)))))
    (while tmh
      (let* ((hop (car tmh))
             (method   (tramp-file-name-method hop))
             (user     (tramp-file-name-user hop))
             (host     (tramp-file-name-host hop))
             (filename (tramp-file-name-localname hop)))
        (setq persp-tramp-file-name (concat
                                     persp-tramp-file-name
                                     method tramp-postfix-method-format
                                     user tramp-postfix-user-format
                                     host (if (= (string-width filename) 0)
                                              tramp-postfix-hop-format
                                            (concat tramp-postfix-host-format filename)))
              tmh (cdr tmh))))
    persp-tramp-file-name))

;; Load funcs

(defsubst persp-update-frames-window-confs (&optional names-regexp)
  (mapc #'(lambda (f) (if names-regexp
                     (when (string-match-p names-regexp
                                           (safe-persp-name (get-frame-persp f)))
                       (persp-restore-window-conf f))
                   (persp-restore-window-conf f)))
        (persp-frame-list-without-daemon)))

(defmacro persp-car-as-fun-cdr-as-args (lst)
  (let ((kar (gensym)))
    `(let* ((,kar (car-safe ,lst))
            (args (cdr-safe ,lst))
            (fun (or (condition-case err
                         (symbol-function ,kar)
                       (error nil))
                     (symbol-value ,kar))))
       (if (functionp fun)
           (apply fun args)
         (message "[persp-mode] Error: %s is not a function." fun)))))


(defun persp-buffers-from-savelist-0 (savelist)
  (let (ret)
    (mapc #'(lambda (saved-buf)
              (block 'persp-buffer-from-savelist
                (let (tmp)
                  (dolist (l-f persp-load-buffer-functions)
                    (setq tmp (funcall l-f saved-buf))
                    (when tmp
                      (when (eq tmp 'skip)
                        (return-from 'persp-buffer-from-savelist))
                      (when (buffer-live-p tmp)
                        (push tmp ret))
                      (return-from 'persp-buffer-from-savelist))))))
          savelist)
    ret))

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
                     (persp (or (gethash pname phash)
                                (persp-add-new pname phash))))
                (mapc #'(lambda (b)
                          (persp-add-buffer b persp nil))
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
                  (set-persp-parameter 'persp-file persp-file persp))))))
    (persp-car-as-fun-cdr-as-args savelist)))

(defun persps-from-savelist-0 (savelist phash persp-file set-persp-file names-regexp)
  (mapc #'(lambda (pd)
            (persp-from-savelist-0 pd phash (and set-persp-file persp-file)))
        (if names-regexp
            (delete-if-not
             #'(lambda (pd)
                 (string-match names-regexp
                               (or (cadr pd) persp-nil-name)))
             savelist)
          savelist)))

(defun persp-names-from-savelist-0 (savelist)
  (mapcar #'(lambda (pd)
              (or (cadr pd) persp-nil-name)) savelist))

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
        (funcall fun s-list phash persp-file set-persp-file names-regexp)
      (message "[persp-mode] Error: Can not load perspectives from savelist: %s\n\tloaded from %s"
               savelist persp-file))))

(defun persp-list-persp-names-in-file (fname)
  (when (and fname (file-exists-p fname))
    (let* ((buf (find-file-noselect fname))
           (pslist (with-current-buffer buf
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
          (message "[persp-mode] Error: No such file -- %S." p-save-file)
        (let (readed-list)
          (with-current-buffer (find-file-noselect p-save-file)
            (goto-char (point-min))
            (setq readed-list (read (current-buffer)))
            (kill-buffer))
          (persps-from-savelist
           readed-list phash p-save-file set-persp-file names-regexp))))
    (when (eq phash *persp-hash*)
      (persp-update-frames-window-confs names-regexp))))

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
      (setq names (persp-prompt t "to load" nil nil nil available-names))))
  (when names
    (let ((names-regexp (regexp-opt names)))
      (persp-load-state-from-file fname phash names-regexp t))))


(provide 'persp-mode)

;;; persp-mode.el ends here
