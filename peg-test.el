;; -*- lexical-binding: t -*-

(require 'ert)
(require 'peg-plus)

(ert-deftest peg-test-macros ()
  "Test all new defined macros."
  
  ;; arg not in guard is pex, should be wrapped with 'funcall
  (should (equal '(* (and (not (funcall pex)) (any)))
                 (define-peg-rule+ any-to-pex (pex)
                   (* (and (not pex) (any))))))
  
  ;; arg in guard is elisp sexp, should not be wrapped with 'funcall
  (should (equal '(* (and (guard (< (point) pos)) (any)))
                 (define-peg-rule+ any-to-point (pos)                   
                   (* (and (guard (< (point) pos)) (any))))))

  ;; 
  (should (equal '(call any-to-pex (peg (eol)))
                 (define-peg-rule+ any-to-eol ()
                   (any-to-pex (eol))))))

;; (ert-run-tests-interactively 'peg-test-macros)

(provide 'peg-test)
