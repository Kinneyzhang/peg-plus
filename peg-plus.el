;; -*- lexical-binding: t -*-

(require 'peg-macros)

;;; custom rules

(define-peg-rule+ group (pex)
  ;; 捕获 PEX 表达式分组，返回 (start string end)
  (list (region (substring pex))))

(define-peg-rule+ any-to-pex (pex)
  ;; 匹配不满足 PEX 的任意字符为止
  (* (and (not pex) (any))))

(define-peg-rule+ any-to-eol ()
  (any-to-pex (eol)))

(define-peg-rule+ any-to-group (pex)
  ;; 从当前位置开始匹配，并捕获 pex
  (any-to-pex pex) (group pex))

(define-peg-rule+ group-to-pex (pex)
  ;; 从当前位置开始捕获，直到匹配 pex 前为止
  (group (any-to-pex pex)))

(define-peg-rule+ any-to-point (pos)
  ;; 匹配任意字符到 pos 位置为止
  (* (and (guard (< (point) pos)) (any))))

(provide 'peg-plus)
