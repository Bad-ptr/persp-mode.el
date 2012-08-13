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
;; C-x x s for create/switch to persp.
;; C-x x a for add buffer to persp
;; And other


(eval-when-compile (require 'cl))

(require 'easymenu)


(defstruct (perspective
            (:conc-name persp-)
            (:constructor make-persp))
  (name "")
  (buffers nil)
  (window-conf (window-state-get)))


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
(define-key persp-mode-map (kbd "C-x x i") 'persp-import)


;;;###autoload
(define-minor-mode persp-mode
  "Toggle perspective mode.
When active, keeps track of multiple 'perspectives',
named collections of buffers and window configurations."
  nil
  :lighter (:eval (format "%s%.5s" "#" (persp-name(get-frame-persp))))
  :global t
  :keymap persp-mode-map
  (if persp-mode
      (progn
        (setf perspectives-hash (make-hash-table :test 'equal :size 10))
        (persp-add-menu)
        (persp-new "main")
        
        (ad-activate 'switch-to-buffer)
        (ad-activate 'display-buffer)
        (ad-activate 'kill-buffer)
        (add-hook 'after-make-frame-functions 'persp-init-frame)
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
      (unless (equal name "main")
        (lexical-let ((str_name name))
          (easy-menu-add-item persp-minor-mode-menu nil
                              (vconcat (list str_name (lambda ()(interactive)
                                                        (persp-switch str_name)))))
          (easy-menu-add-item persp-minor-mode-menu '("kill")
                              (vconcat (list str_name (lambda ()(interactive)
                                                        (persp-kill str_name)))))))
      persp)))

(defun* persp-add-buffer (buffername &optional (persp (get-frame-persp)))
  (interactive
   (list
    (let ((read-buffer-function nil))
      (read-buffer "Add buffer to perspective: "))))
  (let ((buffer (get-buffer buffername)))
    (unless (or (null buffer) (member buffer (persp-buffers persp)))
      (push buffer (persp-buffers persp)))))

(defun* persp-remove-buffer (buffername  &optional (persp (get-frame-persp)))
  (interactive "bRemove buffer from perspective: \n")
  (let ((buffer (get-buffer buffername)))
    (cond ((not (persp-buffer-in-other-p buffer persp))
           (previous-buffer)
           (bury-buffer buffer))
          ((eq buffer (current-buffer)) (bury-buffer))
          (t (bury-buffer buffer)))
    (setf (persp-buffers persp)
          (remq buffer (persp-buffers persp)))))



(defun persp-names ()
  (sort
   (loop for name being the hash-keys of perspectives-hash
         collect name)
   'string<))


(defun persp-init-frame (frame)
  (select-frame frame)
  (modify-frame-parameters
   frame
   '((persp nil)))
  (let ((persp (gethash "main" perspectives-hash)))
    (unless persp
      (setq persp (persp-new "main")))
    (set-frame-persp persp)
    (persp-activate persp)))


(defun persp-kill (name)
  (interactive "i")
  (unless name (setq name (persp-prompt (persp-name (get-frame-persp)) t)))
  (if (equal name "main")
      (message "Can't kill main persp...")
    (let ((persp (gethash name perspectives-hash)))
      ;; (when (eq persp (get-frame-persp))
      ;;   (if (delq name (persp-names))
      ;;       (persp-switch (car (delq name (persp-names))))
      ;;     (set-frame-persp nil)))
      (when persp
        (loop for buf in (persp-buffers persp)
              do (kill-buffer buf))
        (setf (persp-buffers persp) '())

        (remhash name perspectives-hash) 
        
        (loop for frame in (frame-list)
              if (eq persp (get-frame-persp frame))
              do (if (persp-names)
                     (persp-switch (car (persp-names)) frame)))

        (setf persp nil)
        (remhash name perspectives-hash)
        (easy-menu-remove-item persp-minor-mode-menu nil name)
        (easy-menu-remove-item persp-minor-mode-menu '("kill") name)))
    
    (unless (persp-names)
      (persp-switch "main")
      (setf (persp-buffers (get-frame-persp)) (buffer-list)))))


(defun persp-rename (name)
  (interactive "sNew name:")
  (let ((persp (get-frame-persp))
        (opersp (gethash name perspectives-hash)))
    (if (and (not opersp) name)
        (if persp
            (if (equal (persp-name persp) "main")
                (message "Can't rename main persp...")
              (progn
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
                                                                (persp-kill str_name))))))))
          nil)
      (message "%s %s" "There's alway a persp with that name:" name)))
  nil)

(defun* persp-switch (name &optional (frame (selected-frame)))
  (interactive "i")
  (unless name (setq name (persp-prompt )))
  (if (and (get-frame-persp frame) (equal name (persp-name (get-frame-persp frame))))
      name
    (let ((persp (gethash name perspectives-hash)))
      (if (null persp)
          (progn (setq persp (persp-new name))
                 (persp-activate persp frame)
                 (switch-to-buffer (concat "*scratch* (" name ")"))
                 (funcall initial-major-mode))
          (persp-activate persp frame)))
    name))

(defun* persp-activate (persp &optional (frame (selected-frame)))
  (persp-save)
  (persp-save persp)
  (let ((oldf (selected-frame)))
    
    (set-frame-persp persp frame)

    (select-frame frame)
    (delete-other-windows)

    ;; (lexical-let ((oldc (symbol-function #'get-buffer-create)))
    ;;   (flet ((get-buffer-create (bufferorname)
    ;;                             (if (equal bufferorname " *temp*")
    ;;                                 (funcall oldc bufferorname)
    ;;                               (let ((buffer (persp-get-buffer bufferorname)))
    ;;                                 (if buffer
    ;;                                     buffer
    ;;                                   (funcall oldc (concat "*scratch* (" (persp-name persp) ")")))))))
    ;;     (window-state-put (persp-window-conf persp))))
    (if (persp-window-conf persp)
        (window-state-put (persp-window-conf persp))
      (when (persp-buffers persp)
        (switch-to-buffer (car (persp-buffers persp)))))

                                        ;(set-window-configuration (persp-window-configuration persp))
                                        ;(switch-to-buffer (car (persp-buffers persp)))
    (run-hooks 'persp-activated-hook)
    (select-frame oldf)))


(defun* persp-get-buffer (bufferorname &optional (persp (get-frame-persp)))
  ;(message "%s" bufferorname)
  (cond
;   ((null bufferorname) (car (persp-buffers (get-frame-persp))))
   ((stringp bufferorname)
    (loop for buf in (persp-buffers persp)
          if (equal bufferorname (buffer-name buf))
             do (return-from persp-get-buffer buf)))
   ((bufferp bufferorname)
    (loop for buf in (persp-buffers persp)
          if (eq bufferorname (buffer-name buf))
             do (return-from persp-get-buffer buf)))
   (t (car (persp-buffers persp)))))


(defun* persp-save (&optional (persp (get-frame-persp)))
;  (interactive)
  (when persp

    (let ((window nil)
          (frame (selected-frame)))

      (unless (eq persp (get-frame-persp frame))
        (setq frame (find-other-frame-with-persp persp)))

      (when frame
        (setq window (frame-root-window frame)))
      
      (when window
;        (let ((oldframe (selected-frame)))
;          (select-frame frame)
;          (message "%s \n %s" (persp-name persp)(persp-window-conf persp))
          (setf (persp-window-conf persp) (window-state-get window))
;          (message "%s \n %s"(persp-name persp)(persp-window-conf persp))
          ))));)
;          (select-frame oldframe))))))


(defun* find-other-frame-with-persp (&optional (persp (get-frame-persp)) (exframe (selected-frame)))
  (loop for frame in (delete exframe (frame-list))
          if (eq persp (get-frame-persp frame))
            do (return-from find-other-frame-with-persp frame))
  nil)


(defun persp-import-buffers (name)
  (interactive "i")
  (unless name
    (setq name (funcall persp-interactive-completion-function
                        "Import from perspective: " (persp-names) nil t)))
  (if (and (gethash name perspectives-hash)
           (not (equal name (persp-name (get-frame-persp)))))
      (loop for buf in (persp-buffers (gethash name perspectives-hash))
            do (persp-add-buffer (buffer-name buf)))))

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
           (persp-names)
           nil require-match nil nil default))


(defun persp-set-ido-buffers ()
  "Restrict the ido buffer to the current perspective."
  (let ((persp-names
         (remq nil (mapcar 'buffer-name (persp-buffers (get-frame-persp)))))
        (indices (make-hash-table)))
    (let ((i 0))
      (dolist (elt ido-temp-list)
        (puthash elt i indices)
        (setq i (1+ i))))
    (setq ido-temp-list
          (sort persp-names (lambda (a b)
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
  (lexical-let ((persp-names (mapcar 'buffer-name (persp-buffers (get-frame-persp)))))
    (apply-partially 'completion-table-with-predicate
                     (or minibuffer-completion-table 'internal-complete-buffer)
                     (lambda (name)
                       (member (if (consp name) (car name) name) persp-names ))
                     nil)))


(defun* persp-buffer-in-other-p (buffer &optional (persp (get-frame-persp)))
  (loop for p being the hash-values of perspectives-hash
        unless (eq persp p)
          if (member buffer (persp-buffers p))
            return (return-from persp-buffer-in-other-p t) )
  nil)


(defadvice switch-to-buffer (after persp-add-buffer-adv)
  (let ((buf (ad-get-arg 0)))
    (when buf
      (persp-add-buffer buf))))


(defadvice display-buffer (after persp-add-buffer-adv)
    (when ad-return-value
      (let ((buf (ad-get-arg 0)))
        (when buf
          (persp-add-buffer buf)))))

(defadvice kill-buffer (around persp-kill-buffer (&optional b) )
  (let ( (persp (get-frame-persp))
         (buffer (get-buffer b)) )
    
    (if (and persp buffer)
        (let ( (p-buffers (persp-buffers persp)) )

          (if (member buffer p-buffers)
              (progn
                (setf (persp-buffers persp) (delq buffer p-buffers))
                (if (persp-buffers persp)
                    ;(switch-to-buffer (car (persp-buffers persp)))
                    (previous-buffer)
                  
                  (switch-to-buffer (concat "*scratch* (" (persp-name persp) ")")))
                
                (unless (persp-buffer-in-other-p buffer)
                  ad-do-it))

            ad-do-it))
      
      ad-do-it)))

(defadvice display-buffer (after persp-add-buffer-adv)
    (when ad-return-value
      (let ((buf (ad-get-arg 0)))
        (when buf
          (persp-add-buffer buf)))))


(provide 'persp_mode)

;;; persp_mode.el ends here
