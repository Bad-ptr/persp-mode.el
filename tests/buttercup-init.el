(require 'buttercup)

;; use `undercover.el' for coverage report
(when (require 'undercover nil t)
  (undercover "*.el"))

(provide 'buttercup-init)
