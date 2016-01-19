;;; persp-mode.el --- "perspectives" shared among frames + save/load - bugs.

;; Copyright (C) 2012 Constantin Kulikov

;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Version: 1.2
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
(require 'advice)
(require 'easymenu)

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
                              'face (let ((persp (get-frame-persp)))
                                      (if persp
                                          (if (persp-contain-buffer-p (current-buffer) persp)
                                              'persp-face-lighter-default
                                            'persp-face-lighter-buffer-not-in-persp)
                                        'persp-face-lighter-nil-persp)))
                  (safe-persp-name (get-frame-persp))))
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
  :type '(choice (integer :tag "Do not save" :value 0)
                 (integer :tag "Save on exit" :value 1)
                 (integer :tag "Save on exit and persp-mode deactivation"
                          :value 2)))

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
then delete all windows and show the *scratch* buffer."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-set-frame-buffer-predicate t
  "If t -- then set the buffer-predicate frame parameter on perspective
activation."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-switch-to-added-buffer t
  "If t then after you add a buffer to the current perspective
the currently selected window will be switched to that buffer."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-when-kill-switch-to-buffer-in-perspective t
  "If t -- then after a buffer is killed the current window
will be switched to some previous buffer in the current perspective,
otherwise let  the emacs deside what to do."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-ignore-wconf-of-frames-created-to-edit-file t
  "If t -- set the persp-ignore-wconf frame parameter to t for frames
that were created by emacsclient with file arguments.
Also delete windows not showing that files
(this is because server-switch-hook runs after after-make-frames)."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-add-on-switch-or-display nil
  "If not nil then add to the current perspective any buffer which
was switched-to or displayed in any window.
This variable and the switch/display-buffer advices may be removed in a next version."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-add-buffer-on-find-file t
  "If t -- add a buffer with opened file to current perspective."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-kill-foreign-buffer-action 'ask
  "What to do when manually killing a buffer that is not in the current persp.
'ask       -- ask what to do
'kill      -- just kill
<function> -- execute that function. This function will be executed in
  kill-buffer-query-hook, so if it will return nil the buffer will not be killed.
nil        -- do not include the current buffer to buffer list if it not in the perspective."
  :group 'persp-mode
  :type '(choice (const    :tag "Ask what to do" :value ask)
                 (const    :tag "Just kill"      :value kill)
                 (function :tag "Run function"   :value (lambda () t))
                 (const    :tag "do not suggest foreign buffer to the user"
                           :value nil)))

(defcustom persp-filter-save-buffers-functions
  (list #'(lambda (b) (string-prefix-p " " (buffer-name b)))
        #'(lambda (b) (string-prefix-p "*" (buffer-name b))))
  "If one of these functions returns t - a buffer will not be saved."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

(defcustom persp-save-buffer-functions
  (list #'(lambda (b)
            (block 'persp-skip-buffer
              (dolist (f-f persp-filter-save-buffers-functions)
                (when (funcall f-f b)
                  (return-from 'persp-skip-buffer 'skip)))))
        #'(lambda (b)
            (if (or (featurep 'tramp) (require 'tramp nil t))
                (when (tramp-tramp-file-p (buffer-file-name b))
                  `(def-buffer ,(buffer-name b)
                     ,(persp-tramp-save-buffer-file-name b)
                     ,(buffer-local-value 'major-mode b)))
              nil))
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
                                    (message "[persp-mode] Warning: The file %s no longer exists." fname)
                                    (setq buf nil)))
                              (if (and fname (file-exists-p fname))
                                  (setq buf (find-file-noselect fname))
                                (when fname
                                  (message "[persp-mode] Warning: The file %s no longer exists." fname))
                                (setq buf (get-buffer-create name))))
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (typecase mode
                                  (function (when (and (not (eq major-mode mode))
                                                       (not (eq major-mode 'not-loaded-yet)))
                                              (funcall mode))))))
                            buf))))
                (persp-car-as-fun-cdr-as-args savelist (>= . 3))))))
  "Restore a buffer from a saved structure.
If a function return nil -- follow to the next function in the list.
If a function return 'skip -- don't restore a buffer."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

(defcustom persp-mode-hook nil
  "The hook that's run after the `persp-mode' has been activated."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-created-functions nil
  "The list of functions that runs after a perspective has been created.
It's single argument is the created perspective."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

(defcustom persp-before-kill-functions nil
  "The list of functions that runs just before a perspective will be destroyed.
It's single argument is the perspective that will be killed."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

(defcustom persp-before-switch-functions nil
  "The list of functions that runs before actually switching to a perspective.
These functions must take one argument -- a name of a perspective to switch
(it could be a name of an unexistent perspective or it could be the same as current)."
  :group 'persp-mode
  :type '(repeat (function :tag "Function")))

(defcustom persp-activated-hook nil
  "The hook that runs after a perspective has been activated.
Run with the activated perspective as currently active."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-use-workgroups (and (version< emacs-version "24.4")
                                     (locate-library "workgroups.el"))
  "If t -- use the workgroups.el package for saving/restoring windows configurations."
  :group 'persp-mode
  :type 'boolean)

;; require workgroups if we are going to use it
(when persp-use-workgroups
  ;;(require 'workgroups)
  (unless (fboundp 'wg-make-wconfig)
    (autoload 'wg-make-wconfig "workgroups"
      "Return a new Workgroups window config from `selected-frame'." ))
  (unless (fboundp 'wg-restore-wconfig)
    (autoload 'wg-restore-wconfig "workgroups"
      "Restore WCONFIG in `selected-frame'." )))

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
              (flet ((wg-switch-to-window-buffer (win)
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

;; Global variables:

(defvar persp-minor-mode-menu nil
  "Menu for the persp-mode.")

(defvar *persp-hash* nil
  "The hash table that contain perspectives")

(defvar persp-interactive-completion-function
  (cond (ido-mode      #'ido-completing-read)
        (iswitchb-mode #'persp-iswitchb-completing-read)
        (t             #'completing-read))
  "The function which is used by the persp-mode.el
to interactivly read an user input with completion.")

(defvar *persp-restrict-buffers-to* 0
  "The global variable that controls the behaviour of the `persp-buffer-list-restricted'
function (Must be used only for the local rebinding):
0 -- restrict to the current perspective buffers;
1 -- restrict to buffers that is not in the current perspective.")

(defvar persp-saved-read-buffer-function read-buffer-function
  "Save the `read-buffer-function' to restore it on deactivation.")

(defvar persp-nil-wconf nil
  "The window configuration for the `nil' perspective.")

(defvar persp-nil-parameters nil
  "The parameters of the `nil' perspective.")

(defvar persp-last-persp-name persp-nil-name
  "The last activated perspective. A new frame will be created with that perspective
if `persp-set-last-persp-for-new-frames' is t.")

(defvar persp-special-last-buffer nil
  "The special variable to handle the case when new frames switches the selected window buffer
to a wrong one.")

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
(define-key persp-key-map (kbd "s") #'persp-switch)
(define-key persp-key-map (kbd "r") #'persp-rename)
(define-key persp-key-map (kbd "c") #'persp-kill)
(define-key persp-key-map (kbd "a") #'persp-add-buffer)
(define-key persp-key-map (kbd "t") #'persp-temporarily-display-buffer)
(define-key persp-key-map (kbd "i") #'persp-import-buffers)
(define-key persp-key-map (kbd "k") #'persp-remove-buffer)
(define-key persp-key-map (kbd "w") #'persp-save-state-to-file)
(define-key persp-key-map (kbd "l") #'persp-load-state-from-file)
(define-key persp-key-map (kbd "o") #'(lambda ()
                                        (interactive)
                                        (persp-mode -1)))


(defcustom persp-keymap-prefix (kbd "C-c p")
  "The prefix for activating the persp-mode keymap."
  :group 'persp-mode
  :type 'key-sequence
  :set #'(lambda (sym val)
           (when (boundp 'persp-keymap-prefix)
             (substitute-key-definition 'persp-key-map nil persp-mode-map))
           (define-key persp-mode-map val 'persp-key-map)
           (set-default sym val)))

;; Perspective struct:

(defstruct (perspective
            (:conc-name persp-)
            (:constructor make-persp))
  (name "")
  (buffers nil)
  (window-conf nil)
  (parameters nil))

(defun persp-buffer-list (&optional frame)
  (safe-persp-buffers (get-frame-persp frame)))

(defun* persp-buffer-list-restricted (&optional (frame (selected-frame))
                                                (option *persp-restrict-buffers-to*))
  (let ((bl
         (case option
           (0 (persp-buffer-list frame))
           (1 (set-difference (funcall persp-buffer-list-function frame) (persp-buffer-list frame)))))
        (i 1)
        (curbuf (current-buffer))
        cbt kill-found)
    (when (and persp-kill-foreign-buffer-action
               (not (memq curbuf bl)))
      (block pblr-ret
        (while (setq cbt (funcall persp-backtrace-frame-function i 'persp-buffer-list-restricted))
          (if kill-found
              (when (and (eq (car cbt) t)
                         (symbolp (cadr cbt))
                         (string-match-p "^.+?-interactively$" (symbol-name (cadr cbt))))
                (when (eq kill-found (caddr cbt))
                  (setq bl (cons curbuf bl))
                  (set (make-local-variable 'persp-ask-to-kill-buffer-not-in-persp) t))
                (return-from pblr-ret))
            (when (and (eq (car cbt) t)
                       (symbolp (cadr cbt))
                       (interactive-form (cadr cbt))
                       (string-match-p "^.*?kill-buffer.*?$" (symbol-name (cadr cbt))))
              (setq kill-found (cadr cbt))))
          (setq i (1+ i)))))
    bl))

(defmacro* with-persp-buffer-list
    ((&key (buffer-list-function persp-buffer-list-function)
           (restriction *persp-restrict-buffers-to*)
           (frame (selected-frame)))
     &rest body)
  `(flet ((buffer-list (&optional frame)
                       (persp-buffer-list-restricted ,frame ,restriction)))
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

(defun* modify-persp-parameters (alist &optional (persp (get-frame-persp)))
  (loop for (name . value) in alist
        do (set-persp-parameter name value persp)))

(defun* set-persp-parameter (param-name value
                                        &optional (persp (get-frame-persp)))
  (delete-persp-parameter param-name persp)
  (when (and (not (null param-name)) (symbolp param-name))
    (if persp
        (setf (persp-parameters persp)
              (acons param-name value (persp-parameters persp)))
      (setq persp-nil-parameters
            (acons param-name value persp-nil-parameters)))))

(defun* persp-parameter (param-name &optional (persp (get-frame-persp)))
  (if persp
      (cdr-safe (assoc param-name (persp-parameters persp)))
    (cdr-safe (assoc param-name persp-nil-parameters))))

(defun* delete-persp-parameter (param-name &optional (persp (get-frame-persp)))
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
          (persp-add-minor-mode-menu)
          (persp-add-new persp-nil-name)

          (ad-enable-advice #'switch-to-buffer 'after  'persp-add-buffer-adv)
          (ad-enable-advice #'display-buffer   'after  'persp-add-buffer-adv)
          (ad-enable-advice #'kill-buffer      'around 'persp-kill-buffer-adv)
          (ad-activate #'switch-to-buffer)
          (ad-activate #'display-buffer)
          (ad-activate #'kill-buffer)

          (add-hook 'find-file-hook              #'persp-add-or-not-on-find-file)
          (add-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function)
          (add-hook 'before-make-frame-hook      #'persp-before-make-frame)
          (add-hook 'after-make-frame-functions  #'persp-init-new-frame)
          (add-hook 'delete-frame-functions      #'persp-delete-frame)
          (add-hook 'ido-make-buffer-list-hook   #'persp-restrict-ido-buffers)
          (add-hook 'kill-emacs-hook             #'persp-asave-on-exit)
          (when (daemonp)
            (add-hook 'server-switch-hook #'persp-server-switch))

          (setq persp-saved-read-buffer-function  read-buffer-function
                read-buffer-function              #'persp-read-buffer)

          (mapc #'persp-init-frame (persp-frame-list-without-daemon))

          (when (fboundp 'tabbar-mode)
            (setq tabbar-buffer-list-function #'persp-buffer-list))

          (when (fboundp 'iswitchb-mode)
            (add-hook 'iswitchb-make-buflist-hook #'persp-iswitchb-filter-buflist))

          (if (> persp-auto-resume-time 0)
              (run-at-time persp-auto-resume-time nil
                           #'(lambda ()
                               (remove-hook 'find-file-hook #'persp-special-last-buffer-make-current)
                               (when (> persp-auto-resume-time 0)
                                 (persp-load-state-from-file)
                                 (when (buffer-live-p persp-special-last-buffer)
                                   (switch-to-buffer persp-special-last-buffer)))))
            (remove-hook 'find-file-hook #'persp-special-last-buffer-make-current))))

    (when (> persp-auto-save-opt 1) (persp-save-state-to-file))

    (ad-disable-advice #'switch-to-buffer 'after  'persp-add-buffer-adv)
    (ad-disable-advice #'display-buffer   'after  'persp-add-buffer-adv)
    (ad-disable-advice #'kill-buffer      'around 'persp-kill-buffer-adv)
    ;;(ad-deactivate-regexp "^persp-.*")

    (remove-hook 'find-file-hook              #'persp-add-or-not-on-find-file)
    (remove-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function)
    (remove-hook 'before-make-frame-hook      #'persp-before-make-frame)
    (remove-hook 'after-make-frame-functions  #'persp-init-new-frame)
    (remove-hook 'delete-frame-functions      #'persp-delete-frame)
    (remove-hook 'ido-make-buffer-list-hook   #'persp-restrict-ido-buffers)
    (remove-hook 'kill-emacs-hook             #'persp-asave-on-exit)
    (when (daemonp)
      (remove-hook 'server-switch-hook #'persp-server-switch))

    (when (fboundp 'tabbar-mode)
      (setq tabbar-buffer-list-function #'tabbar-buffer-list))
    (when (fboundp 'iswitchb-mode)
      (remove-hook 'iswitchb-make-buflist-hook #'persp-iswitchb-filter-buflist))

    (setq read-buffer-function persp-saved-read-buffer-function
          *persp-hash* nil)))


;; Advices:

(defadvice switch-to-buffer (after persp-add-buffer-adv)
  ;; We must add a buffer to some perspective if we want to display it.
  (when (and persp-add-on-switch-or-display
             persp-mode ad-return-value)
    (let ((buf (ad-get-arg 0)))
      (when buf
        (persp-add-buffer buf (get-frame-persp) nil)))))

(defadvice display-buffer (after persp-add-buffer-adv)
  ;; We must add a buffer to some perspective if we want to display it.
  (when (and persp-add-on-switch-or-display
             persp-mode ad-return-value)
    (let ((buf (ad-get-arg 0)))
      (when buf
        (persp-add-buffer buf (get-frame-persp) nil)))))

(defadvice kill-buffer (around persp-kill-buffer-adv (&optional b) )
  ;; We must remove a buffer from the current perspective before killing it.
  ;; If the buffer belongs to more than one perspective, just remove it from the
  ;; current one and don't kill it. If there is no current perspective
  ;; (or no current frame or current perspective is nil)
  ;; remove the buffer from all perspectives."
  (if persp-mode
      (let ((buffer (or (persp-get-buffer-or-null b)
                        (current-buffer)))
            (persp (get-frame-persp)))
        (if buffer
            (if (string= (buffer-name buffer) "*scratch*")
                (with-current-buffer buffer
                  (message "[persp-mode] Info: This buffer is unkillable in the persp-mode, \
instead it's contents will be erased.")
                  (erase-buffer)
                  (setq ad-return-value nil))
              (let ((persps (persp-persps-with-buffer-except-nil buffer persp))
                    (windows (get-buffer-window-list buffer 0 t))
                    (pbcontain (memq buffer (safe-persp-buffers persp))))
                (if (or (not persp) (not pbcontain))
                    (persp-remove-buffer buffer nil t t)
                  (persp-remove-buffer buffer persp))
                (if (and persp persps pbcontain)
                    (setq ad-return-value nil)
                  (if ad-do-it
                      (setq ad-return-value t)
                    (mapc #'(lambda (p)
                              (persp-add-buffer buffer p nil))
                          (if pbcontain
                              (cons persp persps)
                            persps))
                    (mapc #'(lambda (w)
                              (set-window-buffer w buffer))
                          windows)
                    (setq ad-return-value nil)))))
          ad-do-it))
    ad-do-it))

(defun persp-kill-buffer-query-function ()
  (if (and persp-kill-foreign-buffer-action
           (boundp 'persp-ask-to-kill-buffer-not-in-persp)
           persp-ask-to-kill-buffer-not-in-persp)
      (cond
       ((functionp persp-kill-foreign-buffer-action)
        (funcall persp-kill-foreign-buffer-action))
       ((eq persp-kill-foreign-buffer-action 'kill)
        t)
       (t
        (set (make-local-variable 'persp-ask-to-kill-buffer-not-in-persp) nil)
        (let* ((curbuf (current-buffer))
               (curwin (get-buffer-window curbuf nil))
               (prompt (format "You are going to kill a buffer(%s) which is not in the current perspective. \
It will be removed from every perspective and then killed.\nWhat do you really want to do\
(k - kill/K - kill and close window/c - close window/s - switch to another buffer/q - do nothing)? "
                             (buffer-name curbuf))))
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
            (?s (swb curbuf curwin) nil)
            (t t))))))
    t))

(defun persp-add-or-not-on-find-file ()
  (when persp-add-buffer-on-find-file
    (let ((no-select
           (funcall persp-backtrace-frame-function
                    0 'find-file-noselect)))
      (if no-select
          (let ((persp-switch-to-added-buffer nil))
            (persp-add-buffer (current-buffer)))
        (persp-add-buffer (current-buffer))))))

(defun persp-server-switch ()
  (let* ((cframe (selected-frame))
         (ccp (frame-parameter cframe 'client))
         (bl (when ccp (process-get ccp 'buffers))))
    (when bl
      (set-frame-parameter cframe 'persp-ignore-wconf t)
      (mapc #'(lambda (w)
                (unless (memq (window-buffer w) bl)
                  (delete-window w)))
            (window-list cframe)))))


;; Misc funcs:

(defun persp-get-buffer-or-null (buff-or-name)
  "Safely return a buffer or the nil without errors."
  (typecase buff-or-name
    ((or string buffer)
     (let ((buf (get-buffer buff-or-name)))
       (and (buffer-live-p buf)
            buf)))
    (otherwise nil)))

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

(defun persp-names-current-frame-fast-ordered ()
  (mapcar #'caddr (cddddr persp-minor-mode-menu)))

(defun* persp-get-by-name (name &optional (phash *persp-hash*) default)
  (gethash name phash default))


(defsubst* persp-names-sorted (&optional (phash *persp-hash*))
  (sort (persp-names phash nil) #'string<))

(defsubst persp-regexp-variants (variants &optional begin end)
  (unless begin (setq begin "\\`"))
  (unless end (setq end "\\'"))
  ;;XXX may be use `regexp-opt'?
  (concat begin "\\(" (mapconcat 'identity variants "\\|") "\\)" end))

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

(defun* persp-persps-with-buffer-except-nil
    (buff-or-name
     &optional persp (phash *persp-hash*))
  (let ((buf (persp-get-buffer-or-null buff-or-name)))
    (when buf
      (delete-if-not #'(lambda (p)
                         (when p
                           (memq buf (persp-buffers p))))
                     (delq persp (delq nil (persp-persps phash)))))))

(defun* persp-frames-with-persp (&optional (persp (get-frame-persp)))
  (delete-if-not #'(lambda (f)
                     (eq persp (get-frame-persp f)))
                 (persp-frame-list-without-daemon)))

(defsubst* persp-revive-scratch
    (&optional (persp (get-frame-persp))
               (switchto persp-switch-to-added-buffer))
  "Create and add the *scratch* buffer to a perspective."
  (persp-add-buffer (get-buffer-create "*scratch*") persp switchto))


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
         (pos (position (safe-persp-name (get-frame-persp)) persp-list)))
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
         (pos (position (safe-persp-name (get-frame-persp)) persp-list)))
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
                             (and (eq phash *persp-hash*) (safe-persp-name (get-frame-persp)))
                             t t)))
  (let ((persp (persp-get-by-name name phash))
        (persp-to-switch persp-nil-name))
    (persp-save-state persp)
    (if (and (eq phash *persp-hash*) (null persp))
        (message "[persp-mode] Error: Can't remove 'nil' perspective")
      (remhash name phash)
      (when (eq phash *persp-hash*)
        (persp-remove-from-menu persp)
        (setq persp-to-switch (or (car (persp-names phash nil)) persp-nil-name))
        (mapc #'(lambda (f)
                  (when (eq persp (get-frame-persp f))
                    (persp-switch persp-to-switch f)))
              (persp-frame-list-without-daemon))))
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
          (persp-revive-scratch persp nil)
          (run-hook-with-args 'persp-created-functions persp)
          (persp-add persp phash)))
    (message "[persp-mode] Error: Can't create or switch to a perspective \
with empty name.")
    nil))

(defun* persp-contain-buffer-p (buff-or-name
                                &optional (persp (get-frame-persp)))
  (find (persp-get-buffer-or-null buff-or-name) (safe-persp-buffers persp)))

(defun* persp-add-buffer (buff-or-name
                          &optional (persp (get-frame-persp))
                          (switchorno persp-switch-to-added-buffer))
  (interactive
   (list (let ((*persp-restrict-buffers-to* 1))
           (read-buffer "Add a buffer to the perspective: " (current-buffer)))))
  (let ((buffer (persp-get-buffer-or-null buff-or-name)))
    (when (and persp (buffer-live-p buffer)
               (null (persp-contain-buffer-p buffer persp)))
      (push buffer (persp-buffers persp)))
    (when (and buffer switchorno)
      (switch-to-buffer buffer))
    buffer))

(defun* persp-add-buffers-by-regexp (&optional regexp (persp (get-frame-persp)))
  (interactive)
  (when persp
    (persp-do-buffer-list-by-regexp
     :regexp regexp :func 'persp-add-buffer :rest-args (list persp nil)
     :blist (persp-buffer-list-restricted (selected-frame) 1))))

(defun* persp-temporarily-display-buffer (buff-or-name)
  (interactive (list
                (let ((*persp-restrict-buffers-to* 1))
                  (read-buffer "Temporarily display a buffer, not adding it to the current perspective: "))))
  (let ((buffer (persp-get-buffer-or-null buff-or-name))
        (persp-add-on-switch-or-display nil))
    (when buffer
      (switch-to-buffer buffer t))))

(defun* persp-remove-buffer (buff-or-name
                             &optional (persp (get-frame-persp)) noask-to-remall noswitch)
  "Remove a buffer from a perspective. Switch all windows displaying that buffer
to another one. If `PERSP' is nil -- remove the buffer from all perspectives.
Return the removed buffer."
  (interactive
   (list
    (read-buffer "Remove buffer from perspective: " (current-buffer))))
  (let ((buffer (persp-get-buffer-or-null buff-or-name)))
    ;; (when (buffer-live-p buffer)
    ;;   (bury-buffer buffer))
    (if (null persp)
        (when (or noask-to-remall
                  (yes-or-no-p "Remove buffer from all perspectives?"))
          (mapc #'(lambda (p)
                    (persp-remove-buffer buffer p))
                (persp-persps-with-buffer-except-nil buffer))
          buffer)
      (if (memq buffer (persp-buffers persp))
          (progn
            (setf (persp-buffers persp) (delq buffer (persp-buffers persp)))
            (if noswitch
                buffer
              (persp-switchto-prev-buf buffer persp)))
        nil))))

(defun* persp-remove-buffers-by-regexp (&optional regexp (persp (get-frame-persp)))
  (interactive)
  (when persp
    (persp-do-buffer-list-by-regexp :regexp regexp :func 'persp-remove-buffer
                                    :blist (persp-buffers persp) :rest-args (list persp))))

(defun* persp-import-buffers
    (name
     &optional (persp-to (get-frame-persp)) (phash *persp-hash*))
  "Import buffers from the perspective with the given name to another one.
If run interactively assume import from some perspective that is in the `*persp-hash*'
into the current."
  (interactive "i")
  (unless name
    (setq name (persp-prompt nil "to import buffers from" nil t nil t)))
  (let ((persp-from (persp-get-by-name name phash)))
    (persp-import-buffers-from persp-from persp-to)))

(defun* persp-import-buffers-from (persp-from
                                   &optional (persp-to (get-frame-persp)))
  (if persp-to
      (mapc #'(lambda (b) (persp-add-buffer b persp-to))
            (safe-persp-buffers persp-from))
    (message "[persp-mode] Error: Can't import buffers to the 'nil' perspective, cause it already contain all buffers.")))


(defun* persp-get-buffer (buff-or-name
                          &optional (persp (get-frame-persp)))
  "Like `get-buffer', but constrained to the perspective's list of buffers.
Return the buffer if it's in the perspective or the first buffer from the
perspective buffers or the *scratch* buffer."
  (let ((buffer (persp-get-buffer-or-null buff-or-name)))
    (or (find buffer (safe-persp-buffers persp))
        (first (safe-persp-buffers persp))
        (persp-revive-scratch persp t))))

(defun* persp-buffer-in-other-p
    (buff-or-name
     &optional (persp (get-frame-persp)) (phash *persp-hash*))
  (persp-persps-with-buffer-except-nil buff-or-name persp phash))

(defun* persp-get-another-buffer-for-window (old-buff-or-name window
                                                              &optional (persp (get-frame-persp)))
  (let* ((old-buf (persp-get-buffer-or-null old-buff-or-name))
         (buffers (delete-if #'(lambda (bc)
                                 (or
                                  (eq (car bc) old-buf)
                                  (not (find (car bc) (safe-persp-buffers persp)))))
                             (window-prev-buffers window))))
    (persp-get-buffer (and buffers (car (first buffers))) persp)))

(defun* persp-switchto-prev-buf (old-buff-or-name
                                 &optional (persp (get-frame-persp)))
  "Switch all windows in all frames with a perspective displaying that buffer
to some previous buffer in the perspective.
Return that old buffer."
  (let ((old-buf (persp-get-buffer-or-null old-buff-or-name)))
    (when persp-when-kill-switch-to-buffer-in-perspective
      (mapc #'(lambda (w)
                (set-window-buffer
                 w
                 (persp-get-another-buffer-for-window old-buf w)))
            (delete-if-not #'(lambda (w)
                               (eq (get-frame-persp (window-frame w)) persp))
                           (get-buffer-window-list old-buf nil t))))
    old-buf))

(defsubst* persp-filter-out-bad-buffers (&optional (persp (get-frame-persp)))
  ;; filter out killed buffers
  (when persp
    (delete-if-not #'buffer-live-p (persp-buffers persp))))

(defun persp-kill (name)
  (interactive "i")
  (unless name
    (setq name (persp-prompt nil "to kill" (safe-persp-name (get-frame-persp)) t)))
  (when (or (not (string= name persp-nil-name))
            (yes-or-no-p "Really kill the 'nil' perspective\
(It'l kill all buffers)?"))
    (let ((persp (persp-get-by-name name *persp-hash* :+-123emptynooo))
          (cpersp (get-frame-persp)))
      (unless (eq persp :+-123emptynooo)
        (persp-switch name)
        (run-hook-with-args 'persp-before-kill-functions persp)
        (mapc #'kill-buffer (safe-persp-buffers persp))
        (persp-switch (safe-persp-name cpersp))
        (persp-remove-by-name name)))))

(defun persp-kill-without-buffers (name)
  (interactive "i")
  (unless name
    (setq name (persp-prompt nil "to kill(not killing buffers)"
                             (safe-persp-name (get-frame-persp)) t)))
  (when (not (string= name persp-nil-name))
    (let ((persp (gethash name *persp-hash* :+-123emptynooo))
          (cpersp (get-frame-persp)))
      (unless (eq persp :+-123emptynooo)
        (persp-switch name)
        (run-hook-with-args 'persp-before-kill-functions persp)
        (persp-switch (safe-persp-name cpersp))
        (persp-remove-by-name name)))))

(defun* persp-rename (newname
                      &optional (persp (get-frame-persp)) (phash *persp-hash*))
  (interactive "sNew name: ")
  (let ((opersp (gethash newname phash))
        (old-name (safe-persp-name persp)))
    (if (and (not opersp) newname)
        (progn
          (persp-remove-from-menu persp)
          (remhash old-name phash)
          (if persp
              (setf (persp-name persp) newname)
            (message "[persp-mode] Info: You can't rename the nil perspective, use \
M-x: customize-variable RET persp-nil-name RET"))
          (puthash newname persp phash)
          (persp-add-to-menu persp))
      (message "[persp-mode] Error: There is already a perspective with \
that name: %s." newname)
      nil)))

(defun* persp-switch (name
                      &optional (frame (selected-frame)))
  "Switch to the perspective with name `NAME'.
If there is no perspective with that name it will be created.
Return `NAME'."
  (interactive "i")
  (unless name
    (setq name (persp-prompt nil "to switch to" nil nil nil t)))
  (run-hook-with-args 'persp-before-switch-functions name)
  (if (string= name (safe-persp-name (get-frame-persp frame)))
      name
    (persp-frame-save-state frame)
    (if (string= persp-nil-name name)
        (persp-activate nil frame)
      (let ((p (gethash name *persp-hash*)))
        (persp-activate (or p (persp-add-new name)) frame))))
  name)

(defun persp-before-make-frame ()
  (let ((persp (gethash (or (and persp-set-last-persp-for-new-frames
                                 persp-last-persp-name)
                            persp-nil-name) *persp-hash* :+-123emptynooo)))
    (when (eq persp :+-123emptynooo)
      (setq persp (persp-add-new persp-nil-name)))
    (persp-save-state persp nil t)))

(defun* persp-activate (persp
                        &optional (frame (selected-frame)) new-frame)
  (when frame
    (setq persp-last-persp-name (safe-persp-name persp))
    (set-frame-persp persp frame)
    (persp-restore-window-conf frame persp new-frame)
    (with-selected-frame frame
      (run-hooks 'persp-activated-hook))))

(defun persp-init-new-frame (frame)
  (persp-init-frame frame t))
(defun* persp-init-frame (frame &optional new-frame)
  (let ((persp (gethash (or (and persp-set-last-persp-for-new-frames
                                 persp-last-persp-name)
                            persp-nil-name) *persp-hash* :+-123emptynooo)))
    (modify-frame-parameters frame `((persp . nil)
                                     (buffer-predicate
                                      . ,(persp-make-frame-buffer-predicate frame))))
    (when (eq persp :+-123emptynooo)
      (setq persp (persp-add-new persp-nil-name)))
    (persp-activate persp frame new-frame)))

(defun persp-delete-frame (frame)
  (unless (frame-parameter frame 'persp-ignore-wconf)
    (let ((persp (get-frame-persp frame)))
      (persp-frame-save-state frame
                              (if persp-set-last-persp-for-new-frames
                                  (string= (safe-persp-name persp) persp-last-persp-name)
                                (null persp))))))

(defun* find-other-frame-with-persp (&optional (persp (get-frame-persp))
                                               (exframe (selected-frame))
                                               for-save)
  (let* ((flist (delq exframe (persp-frame-list-without-daemon)))
         (pos (position persp flist
                        :test #'(lambda (p f)
                                  (if for-save
                                      (not (frame-parameter f 'persp-ignore-wconf))
                                    t)
                                  (and f (eq p (get-frame-persp f)))))))
    (and pos (elt flist pos))))


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

(defun persp-prompt (multiple action &optional default require-match delnil delcur persp-list)
  (let ((persps (or persp-list
                    (persp-names-current-frame-fast-ordered))))
    (when delnil
      (setq persps (delete persp-nil-name persps)))
    (when delcur
      (setq persps (delete (safe-persp-name (get-frame-persp)) persps)))
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

(defun persp-make-frame-buffer-predicate (frame)
  (lexical-let ((oldpred (frame-parameter frame 'buffer-predicate)))
    (if persp-set-frame-buffer-predicate
        (lexical-let ((newpred
                       #'(lambda (b)
                           (if persp-mode
                               (memq b (safe-persp-buffers (get-frame-persp)))
                             b))))
          (if oldpred
              #'(lambda (b) (and (funcall oldpred b) (funcall newpred b)))
            newpred))
      oldpred)))

(defun persp-iswitchb-completing-read (prompt choices
                                              &optional predicate require-match
                                              initial-input hist def inherit-input-method)
  "Support for the `iswitchb-mode'."
  (let ((iswitchb-make-buflist-hook
         #'(lambda () (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt def require-match initial-input nil)))

(defun persp-iswitchb-filter-buflist ()
  "Support for the `iswitchb-mode'."
  (when (get-frame-persp)
    (setq iswitchb-temp-buflist (persp-buffer-list-restricted))))


(defun persp-restrict-ido-buffers ()
  "Support for the `ido-mode'."
  (when (get-frame-persp)
    (let ((buffer-names-sorted
           (mapcar #'buffer-name (persp-buffer-list-restricted)))
          (indices (make-hash-table)))
      (let ((i 0))
        (dolist (elt ido-temp-list)
          (puthash elt i indices)
          (setq i (1+ i))))
      (setq ido-temp-list
            (sort buffer-names-sorted #'(lambda (a b)
                                          (< (gethash a indices 10000)
                                             (gethash b indices 10000))))))))

(defun persp-read-buffer (prompt
                          &optional def require-match)
  "Support for the standard read-buffer."
  (cond
   (ido-mode (ido-read-buffer prompt def require-match))
   (iswitchb-mode (iswitchb-read-buffer prompt def require-match))
   (t
    (let* ((read-buffer-function nil)
           (rb-completion-table (persp-complete-buffer))
           (persp-read-buffer-hook
            #'(lambda () (setq minibuffer-completion-table rb-completion-table))))
      (unwind-protect
          (progn
            (add-hook 'minibuffer-setup-hook persp-read-buffer-hook t)
            (read-buffer prompt def require-match))
        (remove-hook 'minibuffer-setup-hook persp-read-buffer-hook))))))

(defun persp-complete-buffer ()
  "Complete buffer."
  (lexical-let ((buffer-names-sorted
                 (mapcar #'buffer-name (persp-buffer-list-restricted))))
    (apply-partially #'completion-table-with-predicate
                     (or minibuffer-completion-table 'internal-complete-buffer)
                     #'(lambda (name)
                         (member (if (consp name) (car name) name)
                                 buffer-names-sorted ))
                     nil)))


;; Save/Load funcs:

(defun persp-list-persp-names-in-file (fname)
  (when (and fname (file-exists-p fname))
    (let* ((buf (find-file-noselect fname))
           (pslist (with-current-buffer buf
                     (goto-char (point-min))
                     (read (current-buffer)))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (mapcar #'(lambda (pd)
                  (or (cadr pd) persp-nil-name)) pslist))))

(defun* persp-restore-window-conf (&optional (frame (selected-frame))
                                             (persp (get-frame-persp frame))
                                             new-frame)
  (when (and frame (not (frame-parameter frame 'persp-ignore-wconf)))
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
                  (let ((persp-add-on-switch-or-display nil))
                    (delete-other-windows)
                    (condition-case err
                        (funcall persp-window-state-put-function pwc frame)
                      (error (message "[persp-mode] Warning: Could not restore window confiuration, because of the error -- %s" err)))
                    (when (and new-frame persp-is-ibc-as-f-supported)
                      (setq initial-buffer-choice #'(lambda () persp-special-last-buffer))))
                (when persp-reset-windows-on-nil-window-conf
                  (delete-other-windows)
                  (persp-revive-scratch persp t)))))
          (when gr-mode
            (golden-ratio-mode 1)))))))


(defun* persp-frame-save-state (&optional (frame (selected-frame)) set-persp-special-last-buffer)
  (let ((persp (get-frame-persp frame)))
    (when (and frame
               (not (persp-is-frame-daemons-frame frame))
               (not (frame-parameter frame 'persp-ignore-wconf)))
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
     ,(persp-parameters-to-savelist persp)))

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
  (interactive (list (read-file-name "Save perspectives to file: "
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
does not exist or not a directory %S." p-save-dir)
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
    (setq names (persp-prompt t "to save" (safe-persp-name (get-frame-persp)) t)))
  (when (or (not fname) (called-interactively-p 'any))
    (setq fname (read-file-name (format "Save subset of perspectives%s to file: "
                                        names)
                                persp-save-dir)))
  (when names
    (unless keep-others
      (setq keep-others (if (and (file-exists-p fname) (yes-or-no-p "Keep other perspectives in the file?"))
                            'yes 'no)))
    (let ((temphash (make-hash-table :test 'equal :size 10))
          bufferlist-pre bufferlist-diff)
      (when (or (eq keep-others 'yes) (eq keep-others t))
        (setq bufferlist-pre (funcall persp-buffer-list-function))
        (persp-load-state-from-file fname temphash (persp-regexp-variants names "[^" "]"))
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

(defmacro persp-preserve-frame (&rest body)
  (let ((c-frame (gensym)))
    `(progn
       (let ((,c-frame (selected-frame)))
         ,@body
         (unless (eq (selected-frame) ,c-frame)
           (select-frame ,c-frame))))))

(defsubst persp-update-frames-window-confs (&optional names-regexp)
  (persp-preserve-frame
   (mapc #'(lambda (f) (if names-regexp
                           (when (string-match-p names-regexp (safe-persp-name (get-frame-persp f)))
                             (persp-restore-window-conf f))
                         (persp-restore-window-conf f)))
         (persp-frame-list-without-daemon))))

(defmacro persp-car-as-fun-cdr-as-args (lst n-args &rest body)
  (let ((kar (gensym)))
    `(let* ((,kar (car-safe ,lst))
            (args (cdr-safe ,lst))
            (fun (symbol-value ,kar)))
       (when (and fun ,(if (consp n-args)
                           `(,(car n-args) (length args) ,(cdr n-args))
                         `(= (length args) ,n-args))
                  (functionp fun))
         (let ((result (apply fun args)))
           ,(if body
                (cons 'progn body)
              'result))))))

(defun persp-buffers-from-savelist (savelist)
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

(defun persp-window-conf-from-savelist (savelist)
  (let ((def-wconf #'identity))
    (persp-car-as-fun-cdr-as-args savelist 1)))

(defun persp-parameters-from-savelist (savelist)
  (let ((def-params #'identity))
    (persp-car-as-fun-cdr-as-args savelist 1)))

(defun persp-from-savelist (savelist phash persp-file)
  (let ((def-persp
          #'(lambda (name dbufs dwc &optional dparams)
              (let* ((pname (or name persp-nil-name))
                     (persp (or (gethash pname phash)
                                (persp-add-new pname phash))))
                (mapc #'(lambda (b)
                          (persp-add-buffer b persp nil))
                      (persp-buffers-from-savelist dbufs))
                (if persp
                    (setf (persp-window-conf persp)
                          (persp-window-conf-from-savelist dwc))
                  (setq persp-nil-wconf
                        (persp-window-conf-from-savelist dwc)))
                (modify-persp-parameters (persp-parameters-from-savelist dparams)
                                         persp)
                (when persp-file
                  (set-persp-parameter 'persp-file persp-file persp))))))
    (persp-car-as-fun-cdr-as-args savelist (>= . 3))))

(defun persps-from-savelist (savelist phash persp-file)
  (mapc #'(lambda (pd) (persp-from-savelist pd phash persp-file)) savelist))

(defun* persp-load-state-from-file (&optional (fname persp-auto-save-fname) (phash *persp-hash*)
                                              names-regexp set-persp-file)
  (interactive (list (read-file-name "Load perspectives from file: "
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
           (if names-regexp
               (delete-if-not #'(lambda (pd)
                                  (string-match-p names-regexp (or (cadr pd)
                                                                   persp-nil-name)))
                              readed-list)
             readed-list)
           phash (and set-persp-file p-save-file)))))
    (when (eq phash *persp-hash*)
      (persp-update-frames-window-confs names-regexp))))

(defun* persp-load-from-file-by-names (&optional (fname persp-auto-save-fname)
                                                 (phash *persp-hash*)
                                                 names)
  (interactive (list (read-file-name "Load subset of perspectives from file: "
                                     persp-save-dir)))
  (unless names
    (let* ((p-save-file (concat (or (file-name-directory fname)
                                    (expand-file-name persp-save-dir))
                                (file-name-nondirectory fname)))
           (available-names (persp-list-persp-names-in-file p-save-file)))
      (setq names (persp-prompt t "to load" nil nil nil available-names))))
  (when names
    (let ((names-regexp (persp-regexp-variants names)))
      (persp-load-state-from-file fname phash names-regexp t))))


(provide 'persp-mode)

;;; persp-mode.el ends here
