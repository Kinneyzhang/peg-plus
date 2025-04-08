;; -*- lexical-binding: t -*-

(require 'peg)
;; (require 'peg-macros)
(require 'peg-utils)

(with-eval-after-load 'peg
  (put 'define-peg-rule 'lisp-indent-function 'defun)
  (put 'define-peg-ruleset 'lisp-indent-function 'defun))



;; (defun peg-group-check-rule ()
;;   "检查在同一个 peg-run 的规则里，group 参数是否全部设置 prop
;; 全部不设置。不允许部分设置 keyword。"
;;   )




(peg-run+ (until (peg "emacs")))
;; emacs
(define-peg-rule group-before (pex &optional prop)
  ;; match any chars before pex and group.
  (group (peg (before pex)) prop))

(define-peg-rule before-point (pex pos)
  ;; search PEX before point POS.
  (until pex) (to pos))

(define-peg-rule group-until (pex &optional prop)
  ;; match any chars before pex and group.
  (group (peg (until pex)) prop))

(define-peg-rule group-pex (pex &optional prop)
  ;; 从当前位置开始匹配，并捕获 pex
  (before pex) (group pex prop))

(define-peg-rule group-pex-to (pex pos &optional prop)
  ;; 从当前位置匹配所有 PEX 到 POS 位置为止
  (+ (and (group-pex pex prop)
          (guard (< (point) pos)))))

(define-peg-rule group-to (pos &optional prop)
  ;; 匹配任意字符到 pos 位置为止
  (group (peg (to pos)) prop))

(define-peg-rule n (point n)
  ;; match N times any chars, POINT must be (point)
  (* (and (guard (< (point) (+ point n))) (any))))

(defun peg-eval (&optional pex)
  (interactive "sInput PEX: ")
  (eval-expression
   `(peg-run (peg ,(car (read-from-string pex))))))

(provide 'peg-plus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
