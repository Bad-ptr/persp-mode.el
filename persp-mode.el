;;; persp-mode.el --- "perspectives" shared between frames

;; Copyright (C) 2012 Constantin Kulikov

;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Version: 0.9.1
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

;;; Usage:

;; Put this file into your load-path,
;; add (require 'persp-mode) (persp-mode t) into your ~/.emacs.
;; To be able to save/restore window configurations to/from file you
;; need workgroups.el

;;; Keys:

;; C-x x s -- create/switch to perspective.
;; C-x x r -- rename perspective.
;; C-x x c -- kill perspective.
;; C-x x a -- add buffer to perspective.
;; C-x x i -- import all buffers from another perspective.
;; C-x x k -- remove buffer from perspective.
;; C-x x w -- save perspectives to file.
;; C-x x l -- load perspectives from file.

;;; Commentary:

;; Based on perspective.el by Nathan Weizenbaum
;; (http://github.com/nex3/perspective-el) but perspectives shared
;; between frames + ability to save/restore perspectives to/from file.


;;; Code:

(eval-when-compile (require 'cl))


(require 'easymenu)


(defstruct (perspective
            (:conc-name persp-)
            (:constructor make-persp))
  (name "")
  (buffers nil)
  (window-conf nil))


(defgroup persp-mode nil
  "Customization of persp-mode."
  :prefix "persp-"
  :group 'persp-mode)

(defcustom persp-save-dir (expand-file-name "~/.emacs.d/persp-confs")
  "Directory to/from where perspectives saved/loaded"
  :group 'persp-mode
  :type 'directory :tag "Directory")

(defcustom persp-auto-save-fname "persp-auto-save"
  "name of file for ato saving perspectives on persp-mode
  deactivation or at emacs exit"
  :group 'persp-mode
  :type '(choice (file :tag "File")))

(defcustom persp-auto-save-opt 2
  "0 -- do not auto save
   1 -- save on exit and only if persp-mode active
   2 -- save on persp-mode deactivation
         or at emacs exiting(if mode is active)"
  :group 'persp-mode
  :type '(choice (integer :tag "Do not save" :value 0)
                 (integer :tag "Save on exit" :value 1)
                 (integer :tag "Save on exit and persp-mode deactivation"
                          :value 2)))

(defcustom persp-auto-resume t
  "if non nil persp will be restored from autosave file
    on mode activation"
  :group 'persp-mode
  :type 'boolean)


(defcustom persp-mode-hook nil
  "A hook that's run after `persp-mode' has been activated."
  :group 'perps-mode
  :type 'hook)

(defcustom persp-created-hook nil
  "A hook that's run after a perspective has been created.
Run with the newly created perspective as `persp-curr'."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-killed-hook nil
  "A hook that's run just before a perspective is destroyed.
Run with the perspective to be destroyed as `persp-curr'."
  :group 'persp-mode
  :type 'hook)

(defcustom persp-activated-hook nil
  "A hook that's run after a perspective has been activated.
Run with the activated perspective active."
  :group 'persp-mode
  :type 'hook)

(defvar persp-mode-map (make-sparse-keymap)
  "Keymap for perspective-mode.")

(defvar persp-minor-mode-menu nil
  "Menu for persp-mode")


(defvar *persp-hash* nil
  "A hash table containing perspectives")

(defvar persp-interactive-completion-function
  (if ido-mode #'ido-completing-read #'completing-read)
  "The function which is used by persp-mode.el
 to interactivly complete user input")


(define-prefix-command 'perspective 'perspective-map)
(define-key persp-mode-map (kbd "C-x x") perspective-map)

(define-key persp-mode-map (kbd "C-x x s") 'persp-switch)
(define-key persp-mode-map (kbd "C-x x r") 'persp-rename)
(define-key persp-mode-map (kbd "C-x x c") 'persp-kill)
(define-key persp-mode-map (kbd "C-x x a") 'persp-add-buffer)
(define-key persp-mode-map (kbd "C-x x i") 'persp-import-buffers)
(define-key persp-mode-map (kbd "C-x x k") 'persp-remove-buffer)

(define-key persp-mode-map (kbd "C-x x w") 'persp-save-state-to-file)
(define-key persp-mode-map (kbd "C-x x l") 'persp-load-state-from-file)


(when (locate-library "workgroups.el")
  (require 'workgroups))

(defun persp-asave-on-exit ()
  (when (> persp-auto-save-opt 0)
    (persp-save-state-to-file persp-auto-save-fname)))

(defun safe-persp-name (p)
  (if p
      (persp-name (get-frame-persp))
    "none"))

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
  :lighter (:eval (format "%s%.5s" "#"
                          (safe-persp-name (get-frame-persp))))
  (if persp-mode
      (progn
        (setf *persp-hash* (make-hash-table :test 'equal :size 10))
        (persp-add-menu)
        
        (ad-activate 'switch-to-buffer)
        (ad-activate 'display-buffer)
        (ad-activate 'kill-buffer)
        
        (add-hook 'after-make-frame-functions #'persp-init-frame)
        (add-hook 'delete-frame-functions     #'persp-delete-frame)
        (add-hook 'ido-make-buffer-list-hook  #'persp-set-ido-buffers)
        (add-hook 'kill-emacs-hook            #'persp-asave-on-exit)

        (setq read-buffer-function #'persp-read-buffer)

        (loop for frame in (frame-list-without-initial)
              do (persp-init-frame frame))
        
        (when (fboundp 'tabbar-mode)
          (setq tabbar-buffer-list-function
                (lambda () (persp-buffers (get-frame-persp)))))

        (when persp-auto-resume
          (persp-load-state-from-file persp-auto-save-fname))

        (run-hooks 'persp-mode-hook))

    (when (> persp-auto-save-opt 1)
      (persp-save-state-to-file persp-auto-save-fname))
    
    (ad-deactivate-regexp "^persp-.*")
    (remove-hook 'after-make-frame-functions #'persp-init-frame)
    (remove-hook 'delete-frame-functions     #'persp-delete-frame)
    (remove-hook 'ido-make-buffer-list-hook  #'persp-set-ido-buffers)
    (remove-hook 'kill-emacs-hook            #'persp-asave-on-exit)

    (setq read-buffer-function nil)
    
    (when (fboundp 'tabbar-mode)
      (setq tabbar-buffer-list-function #'tabbar-buffer-list))

    (setq *persp-hash* nil)))


;; Advices:

(defadvice switch-to-buffer (after persp-add-buffer-adv)
  (when ad-return-value
    (let ((buf (ad-get-arg 0)))
      (when buf
        (persp-add-buffer buf)))))

(defadvice display-buffer (after persp-add-buffer-adv)
  (when ad-return-value
    (let ((buf (ad-get-arg 0)))
      (when buf
        (persp-add-buffer buf)))))

(defadvice kill-buffer (around persp-kill-buffer (&optional b) )
  (let ((buffer (get-buffer b))
        (persp (get-frame-persp))
        (cbuffer (current-buffer)))
    (if (or (null persp) (null buffer))
        ad-do-it
      (if (not (string= (buffer-name buffer) (persp-scratch-name persp)))
          (progn
            (persp-remove-buffer buffer persp)
            (if (not (persp-buffer-in-other-p buffer persp))
                (if ad-do-it
                    (setq ad-return-value t)
                  (persp-add-buffer buffer)
                  (switch-to-buffer cbuffer)
                  (setq ad-return-value nil))
              (setq ad-return-value nil)))
        (message "Error: This buffer is unkillable in persp-mode, instead content of this buffer is erased.")
        (with-current-buffer buffer
          (erase-buffer))
        (setq ad-return-value nil)))))

;; Misc funcs:

(defsubst get-buffer-or-null (b)
  (if (null b)
      nil
    (if (or (bufferp b) (stringp b))
        (get-buffer b)
      nil)))

(defun frame-list-without-initial ()
  (loop for fr in (frame-list)
        if (not (string= "F1" (frame-parameter fr 'name)))
        collect fr))

(defun set-frame-persp (p &optional frame)
  (set-frame-parameter frame 'persp p))

(defun get-frame-persp (&optional frame)
  (frame-parameter frame 'persp))

(defun* persp-names (&optional (ph *persp-hash*))
  (loop for name being the hash-keys of ph
        collect name))

(defsubst* persp-names-sorted (&optional (ph *persp-hash*))
  (sort (persp-names ph) 'string<))

(defun* persp-persps (&optional (ph *persp-hash*))
  (loop for p being the hash-values of ph
        collect p))

(defun persp-persps-with-buffer (b)
  (let ((buf (get-buffer b)))
    (when buf
      (loop for persp in (persp-persps)
            if (member buf (persp-buffers persp))
            collect persp))))

(defun* persp-frames-with-persp (&optional (p (get-frame-persp)))
  (loop for frame in (frame-list-without-initial)
        if (eq p (get-frame-persp frame))
        collect frame))

(defsubst* persp-scratch-name (&optional (p (get-frame-persp)))
  (concat "*scratch* (" (persp-name p) ")"))


;; Perspective funcs:

(defun persp-add (persp)
  (let ((name (persp-name persp)))
    (when name
      (puthash name persp *persp-hash*)
      (lexical-let ((str_name name))
        (easy-menu-add-item persp-minor-mode-menu nil
                            (vconcat (list str_name #'(lambda ()(interactive)
                                                        (persp-switch str_name)))))
        (unless (string= name "main")
          (easy-menu-add-item persp-minor-mode-menu '("kill")
                              (vconcat (list str_name #'(lambda ()(interactive)
                                                          (persp-kill str_name)))))))))
  persp)

(defun persp-remove (name)
  (let ((persp (gethash name *persp-hash*))
        (persp-to-switch nil))
    (when persp
      (easy-menu-remove-item persp-minor-mode-menu nil name)
      (easy-menu-remove-item persp-minor-mode-menu '("kill") name)
      (remhash name *persp-hash*)

      (setq persp-to-switch (or (car (persp-names)) "main"))
      (loop for frame in (frame-list-without-initial)
            if (eq persp (get-frame-persp frame))
            do (persp-switch persp-to-switch frame)))
    (persp-save-state persp)
    persp))


(defun persp-add-new (name)
  (interactive "sName for new perspective: ")
  (when name
    (if (member name (persp-names))
        (message "Error: There is already perspective with %S name." name)
      (let ((persp (make-persp :name name)))
        (setf (persp-buffers persp)
              (list (get-buffer-create (persp-scratch-name persp))))
        (persp-add persp)))))


(defun* persp-add-buffer (bufferorname
                          &optional (persp (get-frame-persp)))
  (interactive
   (list
    (let ((read-buffer-function nil))
      (read-buffer "Add buffer to perspective: "))))
  (when (and persp bufferorname)
    (let ((buffer (get-buffer bufferorname)))
      (when (and buffer
                 (buffer-live-p buffer)
                 (not (member buffer (persp-buffers persp))))
        (push buffer (persp-buffers persp))))))

(defun* persp-remove-buffer (buffername
                             &optional (persp (get-frame-persp)))
  (interactive "bRemove buffer from perspective: \n")
  (let ((buffer (get-buffer buffername)))
    (when (buffer-live-p buffer)
      (bury-buffer buffer))
    (setf (persp-buffers persp) (remq buffer (persp-buffers persp)))
    (switchto-prev-buf-in-persp buffer persp)))

(defun persp-import-buffers (name)
  (interactive "i")
  (unless name
    (setq name (funcall persp-interactive-completion-function
                        "Import from perspective: " (persp-names-sorted) nil)))
  (when (and (gethash name *persp-hash*)
             (not (string= name (persp-name (get-frame-persp)))))
    (persp-import-buffers-from (gethash name *persp-hash*) (get-frame-persp))))

(defun* persp-import-buffers-from (pfrom
                                   &optional (pto (get-frame-persp)))
  (loop for buf in (persp-buffers pfrom)
        do (persp-add-buffer buf pto)))


(defun* persp-get-buffer (bufferorname
                          &optional (persp (get-frame-persp)))
  (let ((buffer (get-buffer bufferorname)))
    (if buffer
        (loop for buf in (persp-buffers persp)
              if (equal buffer buf)
              do (return-from buf))
      (car (persp-buffers persp)))))

(defun* persp-buffer-in-other-p (buffer
                                 &optional (persp (get-frame-persp)))
  (loop for p being the hash-values of *persp-hash*
        unless (eq persp p)
        if (member buffer (persp-buffers p))
        return (return-from persp-buffer-in-other-p t) )
  nil)


(defun* switchto-prev-buf-in-persp (oldbuf
                                    &optional (p (get-frame-persp)))
  (let ((frames (persp-frames-with-persp p))
        new-buf)
    (when (not (persp-buffers p))
      (persp-add-buffer (get-buffer-create (persp-scratch-name persp)) p))
    (loop for frame in frames
          do (loop for window in (get-buffer-window-list oldbuf nil frame)
                   do (progn
                        (loop named obufl for buf in (window-prev-buffers window)
                              if (member buf (persp-buffers p))
                              do (progn
                                   (setq new-buf buf)
                                   (return-from obufl)))
                        (if (not new-buf)
                            (set-window-buffer window (car (persp-buffers p)))
                          (set-window-buffer window new-buf)
                          (setq new-buf nil)))))))

(defsubst persp-filter-out-bad-buffers (persp)
  (when persp
    (loop for buf in (persp-buffers persp)
          if (or (null buf) (not (buffer-live-p buf)))
          do (setf (persp-buffers persp) (remq buf (persp-buffers persp))))))


(defun persp-kill (name)
  (interactive "i")
  (unless name
    (setq name (persp-prompt (persp-name (get-frame-persp)) t)))
  (if (string= "main" name)
      (message "Error: Can't kill main perspective.")
    (persp-remove name)
    (loop for buf in (persp-buffers (persp-remove name))
          do (kill-buffer buf))))


(defun persp-rename (newname)
  (interactive "sNew name: ")
  (let ((persp (get-frame-persp))
        (opersp (gethash newname *persp-hash*)))
    (if (and (not opersp) newname)
        (if persp
            (if (string= (persp-name persp) "main")
                (message "Error: Can't rename main persp.")
              (setf (persp-name (persp-remove (persp-name persp))) newname)
              (persp-add persp)
              (persp-switch (persp-name persp)))
          nil)
      (message "Error: There's already a perspective with that name: %s." name)))
  nil)

(defun* persp-switch (name
                      &optional (frame (selected-frame)))
  (interactive "i")
  (unless name (setq name (persp-prompt )))
  (if (and (get-frame-persp frame)
           (string= name (persp-name (get-frame-persp frame))))
      name
    (let ((persp (gethash name *persp-hash*)))
      (if persp
          (persp-activate persp frame)
        (setq persp (persp-add-new name))
        (persp-activate persp frame t)
        (switch-to-buffer (persp-scratch-name persp))
        (funcall initial-major-mode)))
    name))

(defun* persp-activate (persp
                        &optional (frame (selected-frame)) (new nil))
  (when persp
    (frame-persp-save-state frame)
    (unless new
      (persp-save-state persp))
    (set-frame-persp persp frame)
    (with-selected-frame frame
      (delete-other-windows))
    (persp-restore-window-conf frame persp)
    (run-hooks 'persp-activated-hook)))

(defun persp-init-frame (frame)
  (let ((persp (gethash "main" *persp-hash*))
        (new nil))
    (modify-frame-parameters
     frame
     '((persp . nil)))
    (unless persp
      (setq persp (persp-add-new "main"))
      (setf (persp-buffers persp) (append (persp-buffers persp) (buffer-list)))
      (setq new t))
    (persp-activate persp frame new)))

(defun persp-delete-frame (frame)
  (persp-save-state (get-frame-persp frame)))

(defun* find-other-frame-with-persp (&optional (persp (get-frame-persp))
                                               (exframe (selected-frame)))
  (loop for frame in (delete exframe (frame-list-without-initial))
        if (and frame persp (eq persp (get-frame-persp frame)))
        do (return-from find-other-frame-with-persp frame))
  nil)

;; Helper funcs:

(defun persp-add-menu ()
  (easy-menu-define persp-minor-mode-menu
    persp-mode-map
    "Menu used when persp-mode is active"
    '("Perspectives"
      "-")))

(defun persp-prompt (&optional default require-match)
  (funcall persp-interactive-completion-function
           (concat "Perspective name"
                   (if default (concat " (default " default ")") "") ": ")
           (persp-names-sorted)
           nil require-match nil nil default))


(defun persp-set-ido-buffers ()
  "Restrict the ido buffer to the current perspective."
  (let ((persp-names-sorted
         (remq nil (mapcar 'buffer-name (persp-buffers (get-frame-persp)))))
        (indices (make-hash-table)))
    (let ((i 0))
      (dolist (elt ido-temp-list)
        (puthash elt i indices)
        (setq i (1+ i))))
    (setq ido-temp-list
          (sort persp-names-sorted (lambda (a b)
                                     (< (gethash a indices 10000)
                                        (gethash b indices 10000)))))))


(defun persp-read-buffer (prompt
                          &optional def require-match)
  (let ((read-buffer-function nil))
    (if current-prefix-arg
        (read-buffer prompt def require-match)
      (let ((rb-completion-table (persp-complete-buffer))
            (persp-read-buffer-hook))
        (setq persp-read-buffer-hook
              (lambda ()
                (remove-hook 'minibuffer-setup-hook persp-read-buffer-hook)
                (setq minibuffer-completion-table rb-completion-table)))
        (unwind-protect
            (progn
              (add-hook 'minibuffer-setup-hook persp-read-buffer-hook t)
              (read-buffer prompt def require-match))
          (remove-hook 'minibuffer-setup-hook persp-read-buffer-hook))))))

(defun persp-complete-buffer ()
  (lexical-let ((persp-names-sorted (mapcar 'buffer-name (persp-buffers (get-frame-persp)))))
    (apply-partially 'completion-table-with-predicate
                     (or minibuffer-completion-table 'internal-complete-buffer)
                     (lambda (name)
                       (member (if (consp name) (car name) name) persp-names-sorted ))
                     nil)))

;; Save/Restore funcs:

(defun* persp-restore-window-conf (&optional (frame (selected-frame))
                                             (persp (get-frame-persp frame)))
  (with-selected-frame frame
    (if (persp-window-conf persp)
        (if (not (fboundp 'wg-restore-wconfig))
            (window-state-put (persp-window-conf persp) (frame-root-window frame) t)
          (wg-restore-wconfig (persp-window-conf persp))
          (when (persp-is-ibc-as-f-supported)
            (lexical-let ((cbuf (current-buffer)))
              (setq initial-buffer-choice #'(lambda () cbuf)))))
      (switch-to-buffer (persp-scratch-name persp) ))))

(defsubst persp-is-ibc-as-f-supported ()
  (not (null
        (assoc 'function
               (cdr (getf (symbol-plist 'initial-buffer-choice) 'custom-type))))))

(defun* frame-persp-save-state (&optional (frame (selected-frame)))
  (let ((persp (get-frame-persp frame)))
    (when persp
      (setf (persp-window-conf persp) (persp-window-state-get frame)))))

(defun* persp-save-state (&optional (persp (get-frame-persp)))
  (when persp
    (let ((frame (selected-frame)))
      (unless (eq persp (get-frame-persp frame))
        (setq frame (find-other-frame-with-persp persp)))
      (when frame
        (frame-persp-save-state frame)))))

(defun* persp-window-state-get (frame
                                &optional (rwin (frame-root-window frame)))
  (when frame
    (if (fboundp 'wg-make-wconfig)
        (with-selected-frame frame
          (let ((wc nil))
            (setq wc (wg-make-wconfig))
            wc))
      (window-state-get rwin))))


(defsubst persp-save-all-persps-state ()
  (loop for p in (persp-persps)
        do (persp-save-state p)))

(defun persp-save-state-to-file (fname)
  (interactive "sSave to file: ")
  (when fname
    (let* ((p-save-dir (expand-file-name persp-save-dir))
           (p-save-file (concat p-save-dir "/" fname)))
      (unless (and (file-exists-p p-save-dir)
                   (file-directory-p p-save-dir))
        (message "Info: Trying to create persp-conf-dir.")
        (make-directory p-save-dir t))
      (if (not (and (file-exists-p p-save-dir)
                    (file-directory-p p-save-dir)))
          (message
           "Error: Can't save perspectives, persp-save-dir does not exist or not a directory %S."
           p-save-dir)
        (persp-save-all-persps-state)

        (cl-flet ((prep-bl-fs (blist)
                              (loop for buf in blist
                                    collect `(def-buffer ,(buffer-name buf)
                                               ,(buffer-file-name buf)
                                               ,(buffer-local-value 'major-mode buf)))))
          
          (let ((pslist (loop for p in (persp-persps)
                              do (persp-filter-out-bad-buffers p)
                              and collect `(def-persp ,(persp-name p)
                                             ,(prep-bl-fs (persp-buffers p))
                                             (def-wconf ,(if (find 'workgroups features)
                                                             (persp-window-conf p)
                                                           nil))))))
            (with-current-buffer (find-file-noselect p-save-file)
              (erase-buffer)
              (goto-char (point-min))
              (insert (format "%s\n" (prin1-to-string pslist)))
              (basic-save-buffer)
              (kill-buffer (current-buffer)))))))))


(defun persp-load-state-from-file (fname)
  (interactive "sLoad from file: ")
  (when fname
    (let ((p-save-file (concat (expand-file-name persp-save-dir)
                               "/" fname)))
      (if (not (file-exists-p p-save-file))
          (message "Error: No such file: %S." p-save-file)
        (cl-letf ((def-wconf (lambda (wc) wc))
                  (def-buffer (lambda (name fname mode)
                                (let ((buf (get-buffer name)))
                                  (if (buffer-live-p buf)
                                      (if (or (null fname) (string= fname (buffer-file-name buf)))
                                          buf
                                        (find-file-noselect fname))
                                    (if fname
                                        (find-file-noselect fname)
                                      (with-current-buffer (get-buffer-create name)
                                        (when (and mode (symbol-function mode))
                                          (funcall (symbol-function mode)))
                                        (current-buffer)))))))
                  (def-persp (lambda (name dbufs dwc)
                               (let ((persp (or (gethash name *persp-hash*)
                                                (persp-add-new name))))
                                 (loop for db in dbufs
                                       do (persp-add-buffer (apply (symbol-value (car db)) (cdr db)) persp))
                                 (setf (persp-window-conf persp) (apply (symbol-value (car dwc)) (cdr dwc)))))))
          (with-current-buffer (find-file-noselect p-save-file)
            (goto-char (point-min))
            (loop for pd in (read (current-buffer))
                  do (apply (symbol-value (car pd)) (cdr pd)))
            (kill-buffer (current-buffer))))))))

(defsubst persp-merge-hash (ph)
  (when ph
    (loop for pfrom in (persp-persps ph)
          do (let* ((mname (persp-name pfrom))
                    (pto (gethash mname *persp-hash*)))
               (persp-filter-out-bad-buffers pfrom)
               (if (not (null pto))
                   (progn
                     (persp-import-buffers-from pfrom pto)
                     (setf (persp-window-conf pto) (persp-window-conf pfrom)))
                 (persp-add pfrom)
                 (persp-update-frames-window-confs))))))

(defsubst persp-update-frames-window-confs ()
  (let ((cframe (selected-frame)))
    (loop for frame in (frame-list-without-initial)
          do (progn
               (select-frame frame)
               (delete-other-windows)
               (persp-restore-window-conf)))
    (select-frame cframe)))


(provide 'persp-mode)


;;; persp-mode.el ends here
