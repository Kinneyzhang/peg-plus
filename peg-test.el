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

(ert-deftest peg-test-wrap-funcall ()
  (should (equal
           '((* (and (not (funcall pex)) (any))))
           (plist-get (peg--rule-arg-plist 'any-to-pex)
                      :new-pexs))))

(ert-deftest peg-test-wrap-peg ()
  (should (equal
           '((any-to-pex (peg (eol))))
           (peg--rule-args-add-peg '((any-to-pex (eol)))))))

(peg--rule-args-add-peg '((any-to-pex (eol))))

(peg--rule-arg-plist 'any-to-pex)
(peg--rule-arg-plist 'any-to-point)
(:pex-args (pos) :exp-args nil :new-pexs ((* (and (guard (< (point) (funcall pos))) (any)))))

(ert-run-tests-interactively 'peg-test-wrap-peg)

(provide 'peg-test)


'((* (and (guard (< (point) pos)) (any))))
