;;; persp-mode.el --- switch between named "perspectives" of the editor

;; Copyright 2012 Constantin Kulikov (Bad_ptr)
;;
;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Keywords: extensions
;; X-URL: https://github.com/Bad-ptr/persp-mode.el

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

;;
;; Based on perspective.el by Nathan Weizenbaum
;; (http://github.com/nex3/perspective-el)
;; Put this file into your load-path, add into your ~/.emacs:
;; (require 'persp-mode) (persp-mode t)
;;
;; C-x x s -- create/switch to persp.
;; C-x x r -- rename persp
;; C-x x c -- kill persp.
;; C-x x a -- add buffer to persp.
;; C-x x i -- import all buffers from other persp.
;; C-x x k -- remove buffer from persp.


(eval-when-compile (require 'cl))

(require 'easymenu)

(when (locate-library "workgroups.el")
  (require 'workgroups))
(when (locate-library "pickel.el")
  (require 'pickel))


(defstruct (perspective
            (:conc-name persp-)
            (:constructor make-persp))
  (name "")
  (buffers nil)
  (window-conf nil))


(defvar perspectives-hash nil
  "A hash table containing perspectives")

(defvar persp-conf-dir (expand-file-name "~/.emacs.d/persp-confs")
  "Directory to/from where perspectives saved/loaded")

(defvar persp-auto-save-name "persp-auto-save"
  "name of file for ato saving perspectives on persp-mode
  deactivation or at emacs exit")

(defvar persp-auto-save-opt 2
  "0 -- do not auto save
   1 -- save on exit and only if persp-mode active
   2 -- save on persp-mode deactivation
         or at emacs exiting(if mode is active)")

(defvar persp-auto-resume t
  "if non nil persp will be restored from autosave file
    on mode activation")

(defvar persp-interactive-completion-function
  (if ido-mode 'ido-completing-read 'completing-read)
  "The function which is used by perspective.el
 to interactivly complete user input")

(defvar persp-mode-hook nil
  "A hook that's run after `persp-mode' has been activated.")

(defvar persp-created-hook nil
  "A hook that's run after a perspective has been created.
Run with the newly created perspective as `persp-curr'.")

(defvar persp-killed-hook nil
  "A hook that's run just before a perspective is destroyed.
Run with the perspective to be destroyed as `persp-curr'.")

(defvar persp-activated-hook nil
  "A hook that's run after a perspective has been activated.
Run with the activated perspective active.")

(defvar persp-mode-map (make-sparse-keymap)
  "Keymap for perspective-mode.")

(defvar persp-minor-mode-menu nil
  "Menu for persp-mode")

(define-prefix-command 'perspective 'perspective-map)
(define-key persp-mode-map (kbd "C-x x") perspective-map)

(define-key persp-mode-map (kbd "C-x x s") 'persp-switch)
(define-key persp-mode-map (kbd "C-x x r") 'persp-rename)
(define-key persp-mode-map (kbd "C-x x c") 'persp-kill)
(define-key persp-mode-map (kbd "C-x x a") 'persp-add-buffer)
(define-key persp-mode-map (kbd "C-x x i") 'persp-import-buffers)
(define-key persp-mode-map (kbd "C-x x k") 'persp-remove-buffer)


(defun persp-asave-on-exit ()
  (persp-save-state-to-file persp-auto-save-name))

(defun safe-persp-name (p)
  (if (null p)
      "none"
    (persp-name (get-frame-persp))))

;;;###autoload
(define-minor-mode persp-mode
  "Toggle perspective mode.
When active, keeps track of multiple 'perspectives',
named collections of buffers and window configurations."
  nil
  :lighter (:eval (format "%s%.5s" "#" (safe-persp-name (get-frame-persp))))
  :global t
  :keymap persp-mode-map
  (if persp-mode
      (progn
        (setf perspectives-hash (make-hash-table :test 'equal :size 10))
        (persp-add-menu)
        ;;(persp-new "main")
        ;;(setf (persp-buffers (gethash "main" perspectives-hash)) (buffer-list))
        
        (ad-activate 'switch-to-buffer)
        (ad-activate 'display-buffer)
        ;; (ad-activate 'get-buffer-create)
        ;; (ad-activate 'create-file-buffer)
        (ad-activate 'kill-buffer)
        
        (add-hook 'after-make-frame-functions 'persp-init-frame)
        (add-hook 'delete-frame-functions 'persp-delete-frame)
        (add-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
        (add-hook 'kill-emacs-hook 'persp-asave-on-exit)
        (setq read-buffer-function 'persp-read-buffer)

        (loop for frame in (frame-list-without-initial)
              do (persp-init-frame frame))
        
        (when (boundp tabbar-mode)
          (setq tabbar-buffer-list-function
                (lambda () (persp-buffers (get-frame-persp)))))

        (run-hooks 'persp-mode-hook)
	(when persp-auto-resume
	  (persp-load-state-from-file persp-auto-save-name)))

    (when (> persp-auto-save-opt 1)
      (persp-save-state-to-file persp-auto-save-name))
    
    (ad-deactivate-regexp "^persp-.*")
    (remove-hook 'after-make-frame-functions 'persp-init-frame)
    (remove-hook 'delete-frame-functions 'persp-delete-frame)
    (remove-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
    (remove-hook 'kill-emacs-hook 'persp-asave-on-exit)

    (when (boundp tabbar-mode)
      (setq tabbar-buffer-list-function 'tabbar-buffer-list))

    (setq read-buffer-function nil)
    (setq perspectives-hash nil)))


(defsubst get-buffer-or-null (b)
  (if (null b)
      nil
    (if (or (bufferp b) (stringp b))
        (get-buffer b)
      nil)))

(defun frame-list-without-initial ()
  (let* ((cframe (selected-frame))
         (nframe (next-frame cframe))
         (ret (list cframe)))
    (while (not (eq nframe cframe))
      (setq ret (cons nframe ret))
      (setq nframe (next-frame nframe)))
    ret))

(defun* set-frame-persp (p &optional (frame nil))
  (set-frame-parameter frame 'persp p))

(defun* get-frame-persp ( &optional (frame nil) )
  (frame-parameter frame 'persp))


(defun persp-new (name)
  (when name
    (let ( (persp (make-persp :name name ) ))
      (puthash name persp perspectives-hash)
      (lexical-let ((str_name name))
        (easy-menu-add-item persp-minor-mode-menu nil
                            (vconcat (list str_name (lambda ()(interactive)
                                                      (persp-switch str_name)))))
        (unless (equal name "main")
          (easy-menu-add-item persp-minor-mode-menu '("kill")
                              (vconcat (list str_name (lambda ()(interactive)
                                                        (persp-kill str_name)))))))
      (when (equal name "main")
        (setf (persp-buffers persp) (buffer-list)))
      persp)))

(defun* persp-add-buffer (bufferorname &optional (persp (get-frame-persp)))
  (interactive
   (list
    (let ((read-buffer-function nil))
      (read-buffer "Add buffer to perspective: "))))
  (when (and (not (null bufferorname)) (not (null persp)))
    (let ((buffer (get-buffer bufferorname)))
      (unless (or (null buffer) (not (buffer-live-p buffer)) (member buffer (persp-buffers persp)))
        (push buffer (persp-buffers persp))))))

(defun* persp-remove-buffer (buffername  &optional (persp (get-frame-persp)))
  (interactive "bRemove buffer from perspective: \n")
  (let ((buffer (get-buffer buffername)))
    (when (buffer-live-p buffer)
      (bury-buffer buffer))
    (setf (persp-buffers persp) (remq buffer (persp-buffers persp)))
    (switchto-prev-buf-in-persp buffer persp)))


(defun* persp-names ()
  (loop for name being the hash-keys of perspectives-hash
        collect name))

(defsubst persp-names-sorted ()
  (sort (persp-names) 'string<))

(defun* persp-persps (&optional (ph perspectives-hash))
  (loop for p being the hash-values of ph
        collect p))

(defun persp-persps-with-buffer (b)
  (let ((buf (get-buffer b)))
    (when buf
      (loop for persp being the hash-values of perspectives-hash
            if (member buf (persp-buffers persp))
            collect persp))))

(defun* persp-frames-with-persp (&optional (p (get-frame-persp)))
  (loop for frame in (frame-list-without-initial)
        if (eq p (get-frame-persp frame))
        collect frame))

(defsubst* persp-scratch-name (&optional (p (get-frame-persp)))
  (concat "*scratch* (" (persp-name p) ")"))


(defun persp-init-frame (frame)
  (let ((persp (gethash "main" perspectives-hash))
        (oldf (selected-frame))
        (new nil))
    (select-frame frame)
    (modify-frame-parameters
     frame
     '((persp . nil)))
    (unless persp
      (setq persp (persp-new "main"))
      (setq new t))
    (persp-activate persp frame new)
    (select-frame oldf)))

(defun persp-delete-frame (frame)
  (persp-save-state (get-frame-persp frame)))

(defun persp-kill (name)
  (interactive "i")
  (unless name (setq name (persp-prompt (persp-name (get-frame-persp)) t)))
  (if (equal name "main")
      (message "Can't kill main persp...")
    (let ((persp (gethash name perspectives-hash)))
      (when persp
        (loop for buf in (persp-buffers persp)
              do (kill-buffer buf))
        (setf (persp-buffers persp) '())
        (remhash name perspectives-hash)
        (easy-menu-remove-item persp-minor-mode-menu nil name)
        (easy-menu-remove-item persp-minor-mode-menu '("kill") name)

        (let* ((pnames (persp-names-sorted))
               (pname (if pnames
                          (car pnames)
                        "main"))
               (oldscratch (persp-scratch-name persp)))
          (loop for frame in (frame-list-without-initial)
                if (eq persp (get-frame-persp frame))
                do (persp-switch pname frame))
          (kill-buffer oldscratch))
        (setf persp nil)))))


(defun persp-rename (name)
  (interactive "sNew name:")
  (let ((persp (get-frame-persp))
        (opersp (gethash name perspectives-hash)))
    (if (and (not opersp) name)
        (if persp
            (if (equal (persp-name persp) "main")
                (message "Can't rename main persp...")
              (easy-menu-remove-item persp-minor-mode-menu nil name)
              (easy-menu-remove-item persp-minor-mode-menu '("kill") name)
              (remhash (persp-name persp) perspectives-hash)
              (puthash name persp perspectives-hash)
              (setf (persp-name persp) name)
              (lexical-let ((str_name name))
                (easy-menu-add-item persp-minor-mode-menu nil
                                    (vconcat (list str_name (lambda ()(interactive)
                                                              (persp-switch str_name)))))
                (easy-menu-add-item persp-minor-mode-menu '("kill")
                                    (vconcat (list str_name (lambda ()(interactive)
                                                              (persp-kill str_name)))))))
          nil)
      (message "%s %s" "There's always a persp with that name:" name)))
  nil)

(defun* persp-switch (name &optional (frame (selected-frame)))
  (interactive "i")
  (unless name (setq name (persp-prompt )))
  (if (and (get-frame-persp frame) (equal name (persp-name (get-frame-persp frame))))
      name
    (let ((persp (gethash name perspectives-hash)))
      (if persp
          (persp-activate persp frame)
        (setq persp (persp-new name))
        (persp-activate persp frame t)
        (switch-to-buffer (persp-scratch-name persp))
        (funcall initial-major-mode)))
    name))

(defun* persp-activate (persp &optional (frame (selected-frame)) (new nil))
  (when persp
    (persp-save-state (get-frame-persp frame))
    (unless new
      (persp-save-state persp))
    (let ((oldf (selected-frame))
          (select-frame frame))
	  (set-frame-persp persp frame)
          (delete-other-windows)
          (set-frame-persp persp frame)
          (persp-restore-window-conf persp)
          (run-hooks 'persp-activated-hook)
          (select-frame oldf))))

(defun* persp-restore-window-conf (&optional (persp (get-frame-persp)))
  (when (persp-window-conf persp)
    (if (fboundp 'wg-restore-wconfig)
        (wg-restore-wconfig (persp-window-conf persp))
      (window-state-put (persp-window-conf persp) (frame-root-window frame) t))))

(defun* persp-get-buffer (bufferorname &optional (persp (get-frame-persp)))
  (let ((buffer (get-buffer bufferorname)))
    (if buffer
        (loop for buf in (persp-buffers persp)
              if (equal buffer buf)
              do (return-from buf))
      (car (persp-buffers persp)))))


(defun* frame-persp-save-state (&optional (frame (selected-frame)))
  (let ((persp (get-frame-persp)))
    (when persp
      (setf (persp-window-conf persp) (persp-window-state-get frame)))))

(defun* persp-save-state (&optional (persp (get-frame-persp)))
  (when persp
    (let ((window nil)
          (frame (selected-frame)))
      (unless (eq persp (get-frame-persp frame))
        (setq frame (find-other-frame-with-persp persp)))
      (when frame
        (frame-persp-save-state frame)))))

(defun* persp-window-state-get (frame &optional (rwin (frame-root-window)))
  (when frame
    (if (fboundp 'wg-make-wconfig)
        (let ((cframe (selected-frame))
              (wc nil))
          (select-frame frame)
          (setq wc (wg-make-wconfig))
          (select-frame cframe)
          wc)
      (window-state-get rwin))))


(defun* find-other-frame-with-persp (&optional (persp (get-frame-persp)) (exframe (selected-frame)))
  (loop for frame in (delete exframe (frame-list-without-initial))
        if (eq persp (get-frame-persp frame))
        do (return-from find-other-frame-with-persp frame))
  nil)


(defun persp-import-buffers (name)
  (interactive "i")
  (unless name
    (setq name (funcall persp-interactive-completion-function
                        "Import from perspective: " (persp-names-sorted) nil)))
  (when (and (gethash name perspectives-hash)
           (not (equal name (persp-name (get-frame-persp)))))
      (persp-import-buffers-from (gethash name perspectives-hash) (get-frame-persp))))

(defun* persp-import-buffers-from (pfrom &optional (pto (get-frame-persp)))
  (loop for buf in (persp-buffers pfrom)
        do (persp-add-buffer buf pto)))

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


(defun persp-read-buffer (prompt &optional def require-match)
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


(defun* persp-buffer-in-other-p (buffer &optional (persp (get-frame-persp)))
  (loop for p being the hash-values of perspectives-hash
        unless (eq persp p)
        if (member buffer (persp-buffers p))
        return (return-from persp-buffer-in-other-p t) )
  nil)


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

;; (defadvice get-buffer-create (after persp-add-buffer-adv)
;;   (let ((buf (get-buffer ad-return-value)))
;;     (wheb buf
;;           persp-add-buffer buf)))

;; (defadvice create-file-buffer (after persp-add-buffer-adv)
;;   (let ((buf (get-buffer ad-return-value)))
;;     (wheb buf
;;           persp-add-buffer buf)))


(defun* switchto-prev-buf-in-persp (oldbuf &optional (p (get-frame-persp)))
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

(defadvice kill-buffer (around persp-kill-buffer (&optional b) )
  (let ((buffer (get-buffer b))
        (persp (get-frame-persp))
        (cbuffer (current-buffer)))
    (if (or (null persp) (null buffer))
        ad-do-it
      (if (not (equal (buffer-name buffer) (persp-scratch-name persp)))
          (progn
            (persp-remove-buffer buffer persp)
            (if (not (persp-buffer-in-other-p buffer persp))
                (if ad-do-it
                    (setq ad-return-value t)
                  (persp-add-buffer buffer)
                  (switch-to-buffer cbuffer)
                  (setq ad-return-value nil))
              (setq ad-return-value nil)))
        (message "This buffer is unkillable in persp-mode ;), instead it's content is erased")
        (set-buffer buffer)
        (erase-buffer)
        (set-buffer cbuffer)
        (setq ad-return-value nil)))))


(defsubst persp-save-all-persps-state ()
  (loop for p in (persp-persps)
        do (persp-save-state p)))

(defun persp-save-state-to-file (fname)
  (interactive "sSave config: ")
  (unless fname (setq fname "default"))
  (unless (and (file-exists-p persp-conf-dir) (file-directory-p persp-conf-dir))
    (message "trying to create persp-conf-dir")
    (make-directory persp-conf-dir t))
  (if (not (and (file-exists-p persp-conf-dir) (file-directory-p persp-conf-dir)))
      (message "Error: can't save persp-state( persp-conf-dir not exist or not a directory %S )" persp-conf-dir)
    (if (not (fboundp 'pickel-to-file))
        (message "You must setup pickel.el to be able to save persps states to file")
      (persp-save-all-persps-state)
      (pickel-to-file (concat persp-conf-dir "/" fname) perspectives-hash))))

(defun persp-load-state-from-file (fname)
  (interactive "sLoad config: ")
  (when fname
    (if (not (file-exists-p (concat persp-conf-dir "/" fname)))
        (message "No such config: %S" fname)
      (let ((ph (unpickel-file (concat persp-conf-dir "/" fname))))
        (persp-merge-hash ph)))))

(defsubst persp-merge-hash (ph)
  (when ph
    (loop for pfrom in (persp-persps ph)
          do (let* ((mname (persp-name pfrom))
                    (pto (gethash mname perspectives-hash)))
               (if (not (null pto))
                   (progn
                     (persp-import-buffers-from pfrom pto)
                     (setf (persp-window-conf pto) (persp-window-conf pfrom)))
                 (persp-filter-out-bad-buffers pfrom)
                 (puthash mname pfrom perspectives-hash)
                 (persp-update-frames-window-confs))))))

(defsubst persp-update-frames-window-confs ()
  (let ((cframe (selected-frame)))
    (loop for frame in (frame-list-without-initial)
          do (progn
               (select-frame frame)
               (delete-other-windows)
               (persp-restore-window-conf)))
    (select-frame cframe)))

(defsubst persp-filter-out-bad-buffers (persp)
  (when persp
    (loop for buf in (persp-buffers persp)
          if (or (null buf) (not (buffer-live-p buf)))
          do (setf (persp-buffers persp) (remq buf (persp-buffers persp))))))

(provide 'persp-mode)

;;; persp-mode.el ends here
