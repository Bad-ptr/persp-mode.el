;; persp-mode.el --- switch between named "perspectives" of the editor

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
;; Based on perspective.el by  Nathan Weizenbaum (http://github.com/nex3/perspective-el)
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'persp-mode)
;;   (persp-mode t)
;;
;; C-x x s -- to create/switch to persp.
;; C-x x r -- rename persp
;; C-x x c -- kill persp.
;; C-x x a -- add buffer to persp.
;; C-x x i -- import all buffers from other persp
;; C-x x k -- remove/kill buffer.


(eval-when-compile (require 'cl))

(require 'easymenu)


(defstruct (perspective
            (:conc-name persp-)
            (:constructor make-persp))
  (name "")
  (buffers nil)
  (window-conf
   ;(window-state-get)
   nil))


(defvar perspectives-hash nil
  "A hash table containing perspectives")

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
(define-key persp-mode-map (kbd "C-x x k") 'persp-remove-buffer)
(define-key persp-mode-map (kbd "C-x x c") 'persp-kill)
(define-key persp-mode-map (kbd "C-x x r") 'persp-rename)
(define-key persp-mode-map (kbd "C-x x a") 'persp-add-buffer)
(define-key persp-mode-map (kbd "C-x x i") 'persp-import-buffers)

;;;###autoload
(define-minor-mode persp-mode
  "Toggle perspective mode.
When active, keeps track of multiple 'perspectives',
named collections of buffers and window configurations."
  nil
  :lighter (:eval (format "%s%.5s" "#" (persp-name (get-frame-persp))))
  :global t
  :keymap persp-mode-map
  (if persp-mode
      (progn
        (setf perspectives-hash (make-hash-table :test 'equal :size 10))
        (persp-add-menu)
        (persp-new "main")
        
        (ad-activate 'switch-to-buffer)
        (ad-activate 'display-buffer)
        ;; (ad-activate 'get-buffer-create)
        ;; (ad-activate 'create-file-buffer)
        (ad-activate 'kill-buffer)
        
        (add-hook 'after-make-frame-functions 'persp-init-frame)
        (add-hook 'delete-frame-functions 'persp-delete-frame)
        (add-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
        (setq read-buffer-function 'persp-read-buffer)

        (loop for frame in (frame-list)
              do (persp-init-frame frame))
        (setf (persp-buffers (get-frame-persp)) (buffer-list))
        
        (when tabbar-mode
          (setq tabbar-buffer-list-function
                (lambda () (persp-buffers (get-frame-persp)))))

        (run-hooks 'persp-mode-hook))
    
    (ad-deactivate-regexp "^persp-.*")
    (remove-hook 'after-make-frame-functions 'persp-init-frame)
    (remove-hook 'delete-frame-functions 'persp-delete-frame)
    (remove-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)

    (when tabbar-mode
      (setq tabbar-buffer-list-function 'tabbar-buffer-list))

    (setq read-buffer-function nil)
    (setq perspectives-hash nil)))


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
      persp)))

(defun* persp-add-buffer (bufferorname &optional (persp (get-frame-persp)))
  (interactive
   (list
    (let ((read-buffer-function nil))
      (read-buffer "Add buffer to perspective: "))))
  (let ((buffer (get-buffer bufferorname)))
    (unless (or (null buffer) (member buffer (persp-buffers persp)))
      (push buffer (persp-buffers persp)))))

(defun* persp-remove-buffer (buffername  &optional (persp (get-frame-persp)))
  (interactive "bRemove buffer from perspective: \n")
  (let ((buffer (get-buffer buffername)))
    (when (buffer-live-p buffer)
      (bury-buffer buffer))
    (setf (persp-buffers persp) (remq buffer (persp-buffers persp)))
    (switchto-prev-buf-in-persp buffer persp)))


(defun persp-names ()
  (loop for name being the hash-keys of perspectives-hash
        collect name))

(defsubst persp-names-sorted ()
  (sort (persp-names) 'string<))

(defun persp-persps ()
  (loop for persp being the hash-values of perspectives-hash
        collect persp))

(defun persp-persps-with-buffer (b)
  (let ((buf (get-buffer b)))
    (when buf
      (loop for persp being the hash-values of perspectives-hash
            if (member buf (persp-buffers persp))
            collect persp))))

(defun* persp-frames-with-persp (&optional (p (get-frame-persp)))
  (loop for frame in (frame-list)
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
     '((persp nil)))
    (unless persp
      (setq persp (persp-new "main"))
      (setq new t))
    (set-frame-persp persp)
    (persp-activate persp frame new)
    (select-frame oldf)))

(defun persp-delete-frame (frame)
  (persp-save (get-frame-persp frame)))

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
        (loop for frame in (frame-list)
              if (eq persp (get-frame-persp frame))
              do (when (persp-names-sorted)
                   (persp-switch (car (persp-names)) frame)))
        (setf persp nil))))
  (unless (persp-names)
    (persp-switch "main")
    (setf (persp-buffers (get-frame-persp)) (buffer-list))))


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
      (message "%s %s" "There's alway a persp with that name:" name)))
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

(defun* persp-activate (persp &optional (frame (selected-frame)) new)
  (persp-save)
  (unless new
    (persp-save persp))
  (let ((oldf (selected-frame)))
    (select-frame frame)
    (delete-other-windows)
    (set-frame-persp persp frame)
    (when (persp-window-conf persp)
      (window-state-put (persp-window-conf persp)))
    (run-hooks 'persp-activated-hook)
    (select-frame oldf)))


(defun* persp-get-buffer (bufferorname &optional (persp (get-frame-persp)))
  (let ((buffer (get-buffer bufferorname)))
    (if buffer
        (loop for buf in (persp-buffers persp)
              if (equal buffer buf)
              do (return-from buf))
      (car (persp-buffers persp)))))


(defun* persp-save (&optional (persp (get-frame-persp)))
  (when persp
    (let ((window nil)
          (frame (selected-frame)))
      (unless (eq persp (get-frame-persp frame))
        (setq frame (find-other-frame-with-persp persp)))
      (when frame
        (setq window (frame-root-window frame)))
      (when window
        (setf (persp-window-conf persp) (window-state-get window))))))

(defun* find-other-frame-with-persp (&optional (persp (get-frame-persp)) (exframe (selected-frame)))
  (loop for frame in (delete exframe (frame-list))
        if (eq persp (get-frame-persp frame))
        do (return-from find-other-frame-with-persp frame))
  nil)


(defun persp-import-buffers (name)
  (interactive "i")
  (unless name
    (setq name (funcall persp-interactive-completion-function
                        "Import from perspective: " (persp-names-sorted) nit)))
  (if (and (gethash name perspectives-hash)
           (not (equal name (persp-name (get-frame-persp)))))
      (loop for buf in (persp-buffers (gethash name perspectives-hash))
            do (persp-add-buffer buf))))

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
      (if (equal (buffer-name buffer) (persp-scratch-name persp))
          (progn
            (message "This buffer is unkillable in persp-mode ;), instead it's content is erased")
            (set-buffer buffer)
            (erase-buffer)
            (set-buffer cbuffer))
        (persp-remove-buffer buffer persp)
        (when (not (persp-buffer-in-other-p buffer persp))
          ad-do-it)))))


(provide 'persp-mode)

;;; persp-mode.el ends here
