;;; persp-mode.el --- "perspectives" + save/load + shared among frames.

;; Copyright (C) 2012 Constantin Kulikov

;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Version: 0.9.98
;; Package-Requires: ((workgroups "0.2.0"))
;; Keywords: perspectives
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

;; Based on perspective.el by Nathan Weizenbaum
;; (http://github.com/nex3/perspective-el) but perspectives shared
;; among frames + ability to save/restore perspectives to/from file.

;; Installation:

;; Put this file into your load-path,
;; add (require 'persp-mode) (persp-mode t) into your ~/.emacs.
;; To be able to save/restore window configurations to/from file you
;; need workgroups.el

;; Dependencies:

;; Ability to save/restore window configurations from/to file depends
;; on workgroups.el(https://github.com/tlh/workgroups.el)

;; Keys:

;; C-x x s -- create/switch to perspective.
;; C-x x r -- rename perspective.
;; C-x x c -- kill perspective
;; (if you try to kill 'none' persp -- it'l kill all opened buffers).
;; C-x x a -- add buffer to perspective.
;; C-x x i -- import all buffers from another perspective.
;; C-x x k -- remove buffer from perspective.
;; C-x x w -- save perspectives to file.
;; C-x x l -- load perspectives from file.

;; Customization:

;; M-x: customize-group RET persp-mode RET


;;; Code:

;; Prerequirements:

(require 'cl)
(require 'advice)
(require 'easymenu)

(when (locate-library "workgroups.el")
  (require 'workgroups))


;; Customization variables:

(unless
    (find 'custom-group (symbol-plist 'session))
  (defgroup session nil
    "Emacs's state(opened files, buffers, windows, etc.)"
    :group 'environment))

(defgroup persp-mode nil
  "Customization of persp-mode."
  :prefix "persp-"
  :group 'session)

(defcustom persp-save-dir (expand-file-name "~/.emacs.d/persp-confs")
  "Directory to/from where perspectives saved/loaded by default.
Autosave file saved and loaded to/from this directory."
  :group 'persp-mode
  :type 'directory :tag "Directory")

(defcustom persp-auto-save-fname "persp-auto-save"
  "Name of file for auto save/load perspectives on persp-mode
deactivation or at emacs exit."
  :group 'persp-mode
  :type '(choice (file :tag "File")))

(defcustom persp-auto-save-opt 2
  "This variable controls autosave functionality of persp-mode:
0 -- do not auto save;
1 -- save on exit and only if persp-mode active;
2 -- save on persp-mode deactivation or at emacs exiting."
  :group 'persp-mode
  :type '(choice (integer :tag "Do not save" :value 0)
                 (integer :tag "Save on exit" :value 1)
                 (integer :tag "Save on exit and persp-mode deactivation"
                          :value 2)))

(defcustom persp-auto-save-num-of-backups 3
  "How many autosave file backups to keep."
  :group 'persp-mode
  :type 'integer)

(defcustom persp-auto-resume t
  "If non nil perspectives will be restored from autosave file
on mode activation."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-set-last-persp-for-new-frames t
  "If nil new frames will be created with 'none' perspective,
otherwise with last activated perspective."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-switch-to-added-buffer t
  "If t then after you add a buffer to the perspective the currently selected
 window will be switched to that buffer."
  :group 'persp-mode
  :type 'boolean)

(defcustom persp-mode-hook nil
  "A hook that's run after `persp-mode' has been activated."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-created-functions nil
  "A list of functions that's run after a perspective has been created.
It's single argument is created persp."
  :group 'persp-mode
  :type '(list function))

(defcustom persp-before-kill-functions nil
  "A list of functions that's run just before a perspective is destroyed.
It's single argument is persp that will be killed."
  :group 'persp-mode
  :type '(list function))

(defcustom persp-activated-hook nil
  "A hook that's run after a perspective has been activated.
Run with the activated perspective active."
  :group 'persp-mode
  :type 'hook)


;; Global variables:

(defvar persp-mode-map (make-sparse-keymap)
  "Keymap for perspective-mode.")

(defvar persp-minor-mode-menu nil
  "Menu for persp-mode.")

(defvar *persp-hash* nil
  "A hash table containing perspectives")

(defvar persp-interactive-completion-function
  (if ido-mode #'ido-completing-read #'completing-read)
  "The function which is used by persp-mode.el
 to interactivly complete user input.")

(defvar *persp-restrict-buffers-to* 0
  "Global var for control behaviour of persp-restrict-ido-buffers.
Must be used only for local rebinding.
0 -- restrict to perspective buffers
1 -- restrict to all buffers except that already in perspective.")

(defvar persp-saved-read-buffer-function nil
  "Save read-buffer-function to restore it on mode deactivation.")

(defvar persp-none-wconf nil
  "Window configuration for 'none' persp.")

(defvar persp-none-parameters nil
  "Parameters of 'none' persp.")

(defvar persp-last-persp-name "none"
  "Last perspective. New frame will be created with that perspective.
(if persp-set-last-persp-for-new-frames is t)")

(defvar persp-is-ibc-as-f-supported
  (not
   (null
    (assoc 'function
           (cdr (getf (symbol-plist 'initial-buffer-choice) 'custom-type)))))
  "t if initial-buffer-choice as function is supported in your emacs,
otherwise nil.")


;; Key bindings:

(define-key persp-mode-map (kbd "C-x x s") #'persp-switch)
(define-key persp-mode-map (kbd "C-x x r") #'persp-rename)
(define-key persp-mode-map (kbd "C-x x c") #'persp-kill)
(define-key persp-mode-map (kbd "C-x x a") #'persp-add-buffer)
(define-key persp-mode-map (kbd "C-x x i") #'persp-import-buffers)
(define-key persp-mode-map (kbd "C-x x k") #'persp-remove-buffer)
(define-key persp-mode-map (kbd "C-x x w") #'persp-save-state-to-file)
(define-key persp-mode-map (kbd "C-x x l") #'persp-load-state-from-file)


;; Perspective struct:

(defstruct (perspective
            (:conc-name persp-)
            (:constructor make-persp))
  (name "")
  (buffers nil)
  (window-conf nil)
  (parameters nil))

(defun safe-persp-name (p)
  (if p
      (persp-name p)
    "none"))

(defun safe-persp-buffers (p)
  (if p
      (persp-buffers p)
    (buffer-list)))

(defun safe-persp-window-conf (p)
  (if p
      (persp-window-conf p)
    persp-none-wconf))

(defun safe-persp-parameters (p)
  (if p
      (persp-parameters p)
    persp-none-parameters))

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
      (setq persp-none-parameters
            (acons param-name value persp-none-parameters)))))

(defun* persp-parameter (param-name &optional (persp (get-frame-persp)))
  (if persp
      (cdr-safe (assoc param-name (persp-parameters persp)))
    (cdr-safe (assoc param-name persp-none-parameters))))

(defun* delete-persp-parameter (param-name &optional (persp (get-frame-persp)))
  (when (and (not (null param-name)) (symbolp param-name))
    (if persp
        (setf (persp-parameters persp)
              (delete (assoc param-name (persp-parameters persp))
                      (persp-parameters persp)))
      (setq persp-none-parameters
            (delete (assoc param-name persp-none-parameters)
                    persp-none-parameters)))))


;; Used in mode defenition:

(defmacro persp-alambda (arglist &rest body)
  "Anaphoric lambda."
  (declare (indent 1))
  `(labels ((self ,arglist ,@body))
     #'self))

(defmacro persp-hook-once (hook arglist &rest body)
  "Hook that autoremove itself after execution."
  (declare (indent 1))
  `(add-hook ,hook (persp-alambda ,arglist
                     ,@body
                     (remove-hook ,hook #'self))))

(defun persp-asave-on-exit ()
  (when (> persp-auto-save-opt 0)
    (persp-save-state-to-file persp-auto-save-fname)))


;; Mode itself:

;;;###autoload
(define-minor-mode persp-mode
  "Toggle perspective mode.
When active, keeps track of multiple 'perspectives',
named collections of buffers and window configurations."
  :require 'persp-mode
  :group 'persp-mode
  :keymap persp-mode-map
  :init-value nil
  :global t
  :lighter (:eval (format "%s%.5s" " #"
                          (safe-persp-name (get-frame-persp))))
  (if persp-mode
      (progn
        (setf *persp-hash* (make-hash-table :test 'equal :size 10))
        (persp-add-minor-mode-menu)
        (persp-add-new "none")
        
        (ad-activate 'switch-to-buffer)
        (ad-activate 'display-buffer)
        (ad-activate 'kill-buffer)
        
        (add-hook 'after-make-frame-functions  #'persp-init-new-frame)
        (add-hook 'delete-frame-functions      #'persp-delete-frame)
        (add-hook 'server-switch-hook          #'persp-server-switch)
        (add-hook 'ido-make-buffer-list-hook   #'persp-restrict-ido-buffers)
        (add-hook 'kill-emacs-hook             #'persp-asave-on-exit)

        (setq persp-saved-read-buffer-function read-buffer-function)
        (setq read-buffer-function #'persp-read-buffer)

        (mapc #'persp-init-frame
              (persp-frame-list-without-daemon))
        
        (when (fboundp 'tabbar-mode)
          (setq tabbar-buffer-list-function
                #'(lambda () (let ((p (get-frame-persp)))
                               (if p
                                   (persp-buffers p)
                                 (tabbar-buffer-list))))))

        (when persp-auto-resume
          (if (persp-frame-list-without-daemon)
              (persp-load-state-from-file)
            (lexical-let ((paso persp-auto-save-opt))
              (setq persp-auto-save-opt 0)
              (persp-hook-once
                  'persp-activated-hook ()
                  (run-at-time 3 nil
                               #'(lambda ()
                                   (persp-load-state-from-file)
                                   (setq persp-auto-save-opt paso)))))))

        (run-hooks 'persp-mode-hook))

    (when (> persp-auto-save-opt 1)
      (persp-save-state-to-file))
    
    ;;(ad-deactivate-regexp "^persp-.*")
    (remove-hook 'after-make-frame-functions  #'persp-init-new-frame)
    (remove-hook 'delete-frame-functions      #'persp-delete-frame)
    (remove-hook 'server-switch-hook          #'persp-server-switch)
    (remove-hook 'ido-make-buffer-list-hook   #'persp-restrict-ido-buffers)
    (remove-hook 'kill-emacs-hook             #'persp-asave-on-exit)

    (setq read-buffer-function persp-saved-read-buffer-function)
    
    (when (fboundp 'tabbar-mode)
      (setq tabbar-buffer-list-function #'tabbar-buffer-list))

    (setq *persp-hash* nil)))


;; Advices:

(defadvice switch-to-buffer (after persp-add-buffer-adv)
  ;; We must add buffer to some perspective if we want to display it.
  (when (and persp-mode ad-return-value)
    (let ((buf (ad-get-arg 0)))
      (when buf
        (persp-add-buffer buf (get-frame-persp) nil)))))

(defadvice display-buffer (after persp-add-buffer-adv)
  ;; We must add buffer to some perspective if we want to display it.
  (when (and persp-mode ad-return-value)
    (let ((buf (ad-get-arg 0)))
      (when buf
        (persp-add-buffer buf (get-frame-persp) nil)))))

(defadvice kill-buffer (around persp-kill-buffer (&optional b) )
  ;; We must remove buffer from perspective before killing it.
  ;; If buffer belongs to more than one perspective, just remove it from
  ;; current perspective and don't kill it. If there is no current perspective
  ;; (or no current frame) remove buffer from all perspectives." 
  (if persp-mode
      (let ((buffer (or (persp-get-buffer-or-null b)
                        (current-buffer)))
            (persp (get-frame-persp)))
        (if buffer
            (if (string= (buffer-name buffer) "*scratch*")
                (with-current-buffer buffer
                  (message "[persp-mode] Info: This buffer is unkillable in persp-mode, \
instead content of this buffer is erased.")
                  (erase-buffer)
                  (setq ad-return-value nil))
              (let ((persps (persp-persps-with-buffer-except-none buffer persp))
                    (windows (get-buffer-window-list buffer 0 t)))
                (persp-remove-buffer buffer persp t)
                (if (and persp persps)
                    (setq ad-return-value nil)
                  (if ad-do-it
                      (setq ad-return-value t)
                    (mapc #'(lambda (p)
                              (persp-add-buffer buffer p nil))
                          (cons persp persps))
                    (mapc #'(lambda (w)
                              (set-window-buffer w buffer))
                          windows)
                    (setq ad-return-value nil)))))
          ad-do-it))
    ad-do-it))


;; Misc funcs:

(defun persp-get-buffer-or-null (buff-or-name)
  "Safely return buffer or nil without errors."
  (typecase buff-or-name
    ((or string buffer)
     (let ((buf (get-buffer buff-or-name)))
       (and (buffer-live-p buf)
            buf)))
    (otherwise nil)))

(defun persp-frame-list-without-daemon ()
  "Return list of frames without daemon's frame."
  (if (daemonp)
      (filtered-frame-list #'(lambda (f)
                               (not (or (string= "F1" (frame-parameter f 'name))
                                        (string= (concatenate 'string "emacs@" (system-name))
                                                 (frame-parameter f 'name))))))
    (frame-list)))

(defun set-frame-persp (persp &optional frame)
  (set-frame-parameter frame 'persp persp))

(defun get-frame-persp (&optional frame)
  (frame-parameter frame 'persp))

(defun* persp-names (&optional (phash *persp-hash*))
  (let ((ret nil))
    (maphash #'(lambda (k p)
                 (push k ret))
             phash)
    ret))

(defsubst* persp-names-sorted (&optional (phash *persp-hash*))
  (sort (persp-names phash) 'string<))

(defun* persp-persps (&optional (phash *persp-hash*))
  (let ((ret nil))
    (maphash #'(lambda (k p)
                 (push p ret))
             phash)
    ret))

(defun* persp-persps-with-buffer-except-none
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
  "Create and add scratch buffer to perspective."
  (persp-add-buffer (get-buffer-create "*scratch*") persp switchto))


;; Perspective funcs:

(defun* persp-add (persp &optional (phash *persp-hash*))
  "Insert perspective to phash.
If we adding to *persp-hash* add entries to mode menu.
Return persp."
  (let ((name (safe-persp-name persp)))
    (puthash name persp phash)
    (when (eq phash *persp-hash*)
      (persp-add-to-menu persp)))
  persp)

(defun* persp-remove (name &optional (phash *persp-hash*))
  "Remove perspective from phash. Save it's state before removing.
If we removing from *persp-hash* remove also menu entries.
Switch all frames with that perspective to another one.
Return removed perspective."
  (let ((persp (gethash name phash))
        (persp-to-switch "none"))
    (persp-save-state persp)
    (if (null persp)
        (message "[persp-mode] Error: Can't remove 'none' perspective")
      (remhash name phash)
      (setq persp-to-switch (or (car (persp-names phash)) "none"))

      (when (eq phash *persp-hash*)
        (persp-remove-from-menu persp)
        (mapc #'(lambda (f)
                  (when (eq persp (get-frame-persp f))
                    (persp-switch persp-to-switch f)))
              (persp-frame-list-without-daemon))))
    persp))

(defun* persp-add-new (name &optional (phash *persp-hash*))
  "Create new perspective with given name. Add it to phash.
Return created perspective."
  (interactive "sName for new perspective: ")
  (if (and name (not (string= "" name)))
      (if (member name (persp-names phash))
          (gethash name phash)
        (let ((persp (if (string= "none" name)
                         nil
                       (make-persp :name name))))
          (persp-revive-scratch persp nil)
          (run-hook-with-args 'persp-created-functions persp)
          (persp-add persp phash)))
    (message "[persp-mode] Error: Can't create or switch to perspective \
with empty string as name.")
    nil))

(defun* persp-contain-buffer-p (buff-or-name
                                &optional (persp (get-frame-persp)))
  (find (persp-get-buffer-or-null buff-or-name) (safe-persp-buffers persp)))

(defun* persp-add-buffer (buff-or-name
                          &optional (persp (get-frame-persp))
                          (switchorno persp-switch-to-added-buffer))
  (interactive
   (list (let ((*persp-restrict-buffers-to* 1))
           (read-buffer "Add buffer to perspective: "))))
  (let ((buffer (persp-get-buffer-or-null buff-or-name)))
    (when (and persp (buffer-live-p buffer)
               (null (persp-contain-buffer-p buffer persp)))
      (push buffer (persp-buffers persp)))
    (when (and buffer switchorno)
      (switch-to-buffer buffer))
    buffer))

(defun* persp-remove-buffer (buff-or-name
                             &optional (persp (get-frame-persp)) noask-to-remall noswitch)
  "Remove buffer from perspective. Switch all windows displaying that buffer
to another one. If persp is nil -- remove buffer from all perspectives.
Return removed buffer."
  (interactive
   (list
    (read-buffer "Remove buffer from perspective: ")))
  (let ((buffer (persp-get-buffer-or-null buff-or-name)))
    (when (buffer-live-p buffer)
      (bury-buffer buffer))    
    (if (null persp)
        (when (or noask-to-remall
                  (yes-or-no-p "Remove buffer from all perspectives?"))
          (mapc #'(lambda (p)
                    (persp-remove-buffer buffer p))
                (persp-persps-with-buffer-except-none buffer))
          buffer)
      (if (memq buffer (persp-buffers persp))
          (progn
            (setf (persp-buffers persp) (delq buffer (persp-buffers persp)))
            (if noswitch
                buffer
              (switchto-prev-buf-in-persp buffer persp)))
        nil))))

(defun* persp-import-buffers
    (name
     &optional (persp-to (get-frame-persp)) (phash *persp-hash*))
  "Import buffers from perspective with given name to another one.
If run interactively assume import from some perspective that in *persp-hash*
into current."
  (interactive "i")
  (unless name
    (setq name (funcall persp-interactive-completion-function
                        "Import from perspective: "
                        (delete (safe-persp-name (get-frame-persp))
                                (persp-names-sorted)) nil)))
  (let ((persp-from (gethash name phash)))
    (persp-import-buffers-from persp-from persp-to)))

(defun* persp-import-buffers-from (persp-from
                                   &optional (persp-to (get-frame-persp)))
  (if persp-to
      (mapc #'(lambda (b)
                (persp-add-buffer b persp-to))
            (safe-persp-buffers persp-from))
    (message "[persp-mode] Error: Can't import buffers to 'none' perspective.")))


(defun* persp-get-buffer (buff-or-name
                          &optional (persp (get-frame-persp)))
  "Like get-buffer, but constrained to perspective's list of buffers.
Return buffer if it's in perspective or return first buffer in persp-buffers
or return perspective's scratch."
  (let ((buffer (persp-get-buffer-or-null buff-or-name)))
    (or (find buffer (safe-persp-buffers persp))
        (first (safe-persp-buffers persp))
        (persp-revive-scratch persp t))))

(defun* persp-buffer-in-other-p
    (buff-or-name
     &optional (persp (get-frame-persp)) (phash *persp-hash*))
  (persp-persps-with-buffer-except-none buff-or-name persp phash))

(defun* switchto-prev-buf-in-persp (old-buff-or-name
                                    &optional (persp (get-frame-persp)))
  "Switch all windows displaying that buffer in all frames with perspective
to some previous buffer in perspective.
Return that old buffer."
  (let ((old-buf (persp-get-buffer-or-null old-buff-or-name)))
    (persp-revive-scratch persp nil)
    (mapc #'(lambda (w)
              (set-window-buffer
               w (persp-get-buffer
                  (first (intersection (safe-persp-buffers persp)
                                       (window-prev-buffers w))) persp)))
          (delete-if-not #'(lambda (w)
                             (eq (get-frame-persp (window-frame w)) persp))
                         (get-buffer-window-list old-buf nil t)))
    old-buf))

(defsubst* persp-filter-out-bad-buffers (&optional (persp (get-frame-persp)))
  ;; filter out killed buffers
  (when persp
    (delete-if-not #'buffer-live-p
                   (persp-buffers persp))))

(defun persp-kill (name)
  (interactive "i")
  (unless name
    (setq name (persp-prompt nil t)))
  (when (or (not (string= name "none"))
            (yes-or-no-p "Really kill 'none' perspective\
(It'l kill all buffers)?"))
    (let ((persp (gethash name *persp-hash* :+-123emptynooo))
          (cpersp (get-frame-persp)))
      (unless (eq persp :+-123emptynooo)
        (persp-switch name)
        (run-hook-with-args 'persp-before-kill-functions persp)
        (mapc #'kill-buffer (safe-persp-buffers persp))
        (persp-switch (safe-persp-name cpersp))
        (persp-remove name)))))

(defun* persp-rename (newname
                      &optional (persp (get-frame-persp)) (phash *persp-hash*))
  (interactive "sNew name: ")
  (let ((opersp (gethash newname phash))
        (old-name (persp-name persp)))
    (if (and (not opersp) newname)
        (if (null persp)
            (message "[persp-mode] Error: Can't rename 'none' perspective")
          (persp-remove-from-menu persp)
          (remhash old-name phash)
          (setf (persp-name persp) newname)
          (puthash newname persp phash)
          (persp-add-to-menu persp))
      (message "[persp-mode] Error: There's already a perspective with \
that name: %s." newname)))
  nil)

(defun* persp-switch (name
                      &optional (frame (selected-frame)))
  "Switch to perspective with name.
If there is no perspective with that name it will be created.
Return name."
  (interactive "i")
  (unless name
    (setq name (funcall persp-interactive-completion-function
                        "Switch to perspective: "
                        (delete (safe-persp-name (get-frame-persp))
                                (persp-names-sorted)) nil)))
  (if (string= name (safe-persp-name (get-frame-persp frame)))
      name
    (persp-frame-save-state frame)
    (if (string= "none" name)
        (persp-activate nil frame)
      (let ((p (gethash name *persp-hash*)))
        (persp-activate (or p (persp-add-new name)) frame))))
  name)

(defun* persp-activate (persp
                        &optional (frame (selected-frame)) new-frame)
  (when frame
    (persp-save-state persp frame)
    (setq persp-last-persp-name (safe-persp-name persp))
    (set-frame-persp persp frame)
    (persp-restore-window-conf frame persp new-frame)
    (run-hooks 'persp-activated-hook)))

(defun persp-init-new-frame (frame)
  (persp-init-frame frame t))
(defun* persp-init-frame (frame &optional new-frame)
  (let ((persp (gethash (or (and persp-set-last-persp-for-new-frames
                                 persp-last-persp-name)
                            "none") *persp-hash* :+-123emptynooo)))
    (modify-frame-parameters
     frame
     '((persp . nil)))
    (when (eq persp :+-123emptynooo)
      (setq persp (persp-add-new "none")))
    (persp-activate persp frame new-frame)))

(defun persp-delete-frame (frame)
  (persp-frame-save-state frame)
  (when persp-is-ibc-as-f-supported
    (setq initial-buffer-choice nil)))

(defun persp-server-switch ()
  (when persp-is-ibc-as-f-supported
    (lexical-let ((cbuf (current-buffer)))
      (setq initial-buffer-choice
            #'(lambda () (setq initial-buffer-choice nil) cbuf)))))

(defun* find-other-frame-with-persp (&optional (persp (get-frame-persp))
                                               (exframe (selected-frame)))
  (find persp (delq exframe (persp-frame-list-without-daemon))
        :test #'(lambda (p f)
                  (and f p (eq p (get-frame-persp f))))))


;; Helper funcs:

(defun persp-add-minor-mode-menu ()
  (easy-menu-define persp-minor-mode-menu
    persp-mode-map
    "Menu used when persp-mode is active"
    '("Perspectives"
      "-")))

(defun persp-remove-from-menu (persp)
  (when persp
    (easy-menu-remove-item persp-minor-mode-menu nil (persp-name persp))
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

(defun persp-prompt (&optional default require-match delnone)
  (funcall persp-interactive-completion-function
           (concat "Perspective name"
                   (if default (concat " (default " default ")") "")
                   ": ")
           (if delnone
               (delete "none" (persp-names-sorted))
             (persp-names-sorted))
           nil require-match nil nil default))

(defun persp-restrict-ido-buffers ()
  "Restrict the ido buffer to the current perspective
if *persp-restrict-buffers-to* is 0.
If *persp-restrict-buffers-to* is 1 restrict to all buffers
except current perspective's buffers."
  (when (get-frame-persp)
    (let ((buffer-names-sorted
           (mapcar 'buffer-name
                   (if (= *persp-restrict-buffers-to* 0)
                       (persp-buffers (get-frame-persp))
                     (set-difference (buffer-list)
                                     (persp-buffers (get-frame-persp))))))
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
  (if ido-mode
      (ido-read-buffer prompt def require-match)
    (let ((read-buffer-function nil))
      (let ((rb-completion-table (persp-complete-buffer))
            (persp-read-buffer-hook
             #'(lambda ()
                 (setq minibuffer-completion-table rb-completion-table))))
        (unwind-protect
            (progn
              (add-hook 'minibuffer-setup-hook persp-read-buffer-hook t)
              (read-buffer prompt def require-match))
          (remove-hook 'minibuffer-setup-hook persp-read-buffer-hook))))))

(defun persp-complete-buffer ()
  "Complete buffer.
If *persp-restrict-buffers-to* is 0 list buffers in perspective.
If *persp-restrict-buffers-to* is 1 list all buffers
except current perspective's buffers."
  (lexical-let ((buffer-names-sorted
                 (if (get-frame-persp)
                     (mapcar #'buffer-name
                             (if (= *persp-restrict-buffers-to* 0)
                                 (persp-buffers (get-frame-persp))
                               (set-difference
                                (buffer-list)
                                (persp-buffers (get-frame-persp)))))
                   (mapcar #'buffer-name (buffer-list)))))
    (apply-partially 'completion-table-with-predicate
                     (or minibuffer-completion-table 'internal-complete-buffer)
                     #'(lambda (name)
                         (member (if (consp name) (car name) name)
                                 buffer-names-sorted ))
                     nil)))


;; Save/Load funcs:

(defun* persp-restore-window-conf (&optional (frame (selected-frame))
                                             (persp (get-frame-persp frame))
                                             new-frame)
  (with-selected-frame frame
    (let ((pwc (safe-persp-window-conf persp))
          (split-width-threshold 8)
          (split-height-threshold 8)
          gratio)
      (delete-other-windows)
      (when (and (fboundp 'golden-ratio-mode) golden-ratio-mode)
        (setq gratio t)
        (golden-ratio-mode -1))
      (if pwc
          (if (not (fboundp 'wg-restore-wconfig))
              (window-state-put pwc (frame-root-window frame) t)
            (wg-restore-wconfig pwc))
        (persp-revive-scratch persp t))
      (when gratio
        (golden-ratio-mode t))
      (when persp-is-ibc-as-f-supported
        (if new-frame
            (lexical-let ((cbuf (current-buffer)))
              (setq initial-buffer-choice
                    #'(lambda () (setq initial-buffer-choice nil) cbuf)))
          (when (functionp initial-buffer-choice)
            (switch-to-buffer (funcall initial-buffer-choice))))))))

(defun* persp-frame-save-state (&optional (frame (selected-frame)))
  (let ((persp (get-frame-persp frame)))
    (when (and frame
               (not (and (daemonp) (string= "F1" (frame-parameter frame 'name))))
               (not (string= (concatenate 'string "emacs@" (system-name))
                             (frame-parameter frame 'name))))
      (if persp
          (setf (persp-window-conf persp) (persp-window-state-get frame))
        (setq persp-none-wconf (persp-window-state-get frame))))))

(defun* persp-save-state (&optional (persp (get-frame-persp)) exfr)
  (let ((frame (selected-frame)))
    (unless (eq persp (get-frame-persp frame))
      (setq frame (find-other-frame-with-persp persp exfr)))
    (when frame
      (persp-frame-save-state frame))))

(defun* persp-window-state-get (frame
                                &optional (rwin (frame-root-window frame)))
  (when frame
    (if (fboundp 'wg-make-wconfig)
        (with-selected-frame frame
          (wg-make-wconfig))
      (window-state-get rwin))))

(defsubst persp-save-all-persps-state ()
  (mapc #'persp-save-state
        (persp-persps)))


;; Save funcs

(defun persp-buffers-to-savelist (persp)
  (mapcar #'(lambda (b)
              `(def-buffer ,(buffer-name b)
                 ,(buffer-file-name b)
                 ,(buffer-local-value 'major-mode b)))
          (delete-if #'(lambda (b)
                         (string-prefix-p " " (buffer-name b)))
                     (safe-persp-buffers persp))))

(defun persp-window-conf-to-savelist (persp)
  `(def-wconf ,(if (find 'workgroups features)
                   (safe-persp-window-conf persp)
                 nil)))

(defun persp-parameters-to-savelist (persp)
  `(def-params ,(remove-if
                 #'(lambda (param)
                     (and (not (stringp param))
                          (string-match-p "#<.*?>"
                                          (prin1-to-string param))
                          (message "[persp-mode] Info: parameter %S \
of perspective %s can't be saved."
                                   param (safe-persp-name persp))
                          t))
                 (safe-persp-parameters persp))))

(defun persp-to-savelist (persp)
  `(def-persp ,(safe-persp-name persp)
     ,(persp-buffers-to-savelist persp)
     ,(persp-window-conf-to-savelist persp)
     ,(persp-parameters-to-savelist persp)))

(defun persps-to-savelist (phash)
  (mapcar #'persp-to-savelist (persp-persps phash)))

(defsubst persp-save-with-backups (fname)
  (when (and (string= fname
                      (concat (expand-file-name persp-save-dir)
                              "/" persp-auto-save-fname))
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

(defun* persp-save-state-to-file (&optional (fname persp-auto-save-fname))
  (interactive (list (read-file-name "Save perspectives to file: "
                                     persp-save-dir)))
  (when fname
    (let* ((p-save-dir (or (file-name-directory fname)
                           (expand-file-name persp-save-dir)))
           (p-save-file (concat p-save-dir "/" (file-name-base fname))))
      (unless (and (file-exists-p p-save-dir)
                   (file-directory-p p-save-dir))
        (message "[persp-mode] Info: Trying to create persp-conf-dir.")
        (make-directory p-save-dir t))
      (if (not (and (file-exists-p p-save-dir)
                    (file-directory-p p-save-dir)))
          (message
           "[persp-mode] Error: Can't save perspectives, persp-save-dir \
does not exist or not a directory %S."
           p-save-dir)
        (persp-save-all-persps-state)
        (with-temp-buffer
          (erase-buffer)
          (goto-char (point-min))
          (insert (let ((print-length nil)
                        (print-level nil))
                    (prin1-to-string (persps-to-savelist *persp-hash*))))
          (persp-save-with-backups p-save-file))))))


;; Load funcs

(defmacro persp-preserve-frame (&rest body)
  (let ((c-frame (gensym)))
    `(progn
       (let ((,c-frame (selected-frame)))
         ,@body
         (select-frame ,c-frame)))))

(defsubst persp-update-frames-window-confs ()
  (persp-preserve-frame
   (mapc #'(lambda (f)
             (select-frame f)
             (delete-other-windows)
             (persp-restore-window-conf))
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
  (let ((def-buffer
          #'(lambda (name fname mode)
              (let ((buf (persp-get-buffer-or-null name)))
                (if (buffer-live-p buf)
                    (if (or (null fname)
                            (string= fname (buffer-file-name buf)))
                        buf
                      (if (file-exists-p fname)
                          (find-file-noselect fname)
                        (message "[persp-mode] Warning: File %s no longer exists." fname)
                        buf))
                  (if (and fname (file-exists-p fname))
                      (find-file-noselect fname)
                    (when fname
                      (message "[persp-mode] Warning: File %s no longer exists." fname))
                    (get-buffer-create name)))))))
    (mapcar #'(lambda (db) (persp-car-as-fun-cdr-as-args db 3))
            savelist)))

(defun persp-window-conf-from-savelist (savelist)
  (let ((def-wconf #'identity))
    (persp-car-as-fun-cdr-as-args savelist 1)))

(defun persp-parameters-from-savelist (savelist)
  (let ((def-params #'identity))
    (persp-car-as-fun-cdr-as-args savelist 1)))

(defun persp-from-savelist (savelist phash)
  (let ((def-persp
          #'(lambda (name dbufs dwc &optional dparams)
              (let ((persp (or (gethash name phash)
                               (persp-add-new name phash))))
                (mapc #'(lambda (b)
                          (persp-add-buffer b persp nil))
                      (persp-buffers-from-savelist dbufs))
                (if persp
                    (setf (persp-window-conf persp)
                          (persp-window-conf-from-savelist dwc))
                  (setq persp-none-wconf
                        (persp-window-conf-from-savelist dwc)))
                (modify-persp-parameters (persp-parameters-from-savelist dparams)
                                         persp)))))
    (persp-car-as-fun-cdr-as-args savelist (>= . 3))))

(defun persps-from-savelist (savelist phash)
  (mapc #'(lambda (pd) (persp-from-savelist pd phash))
        savelist))

(defun* persp-load-state-from-file (&optional (fname persp-auto-save-fname))
  (interactive (list (read-file-name "Load perspectives from file: "
                                     persp-save-dir)))
  (when fname
    (let ((p-save-file (concat (or (file-name-directory fname)
                                   (expand-file-name persp-save-dir))
                               "/" (file-name-base fname))))
      (if (not (file-exists-p p-save-file))
          (message "[persp-mode] Error: No such file: %S." p-save-file)
        (with-current-buffer (find-file-noselect p-save-file)
          (goto-char (point-min))
          (persps-from-savelist (read (current-buffer)) *persp-hash*)
          (kill-buffer (current-buffer)))))
    (persp-update-frames-window-confs)))


(provide 'persp-mode)

;;; persp-mode.el ends here
