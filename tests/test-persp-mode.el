(require 'buttercup-init)
(require 'persp-mode)

(describe "persp-mode"
  (it "activates and deactives without error"
    (expect (lambda () (persp-mode)) :not :to-throw)
    (expect (lambda () (persp-mode -1)) :not :to-throw)))
