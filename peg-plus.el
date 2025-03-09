;; -*- lexical-binding: t -*-

;; (require 'peg-macros)

;;; custom rule

(require 'peg)

(defvar peg-group-data nil
  "A plist data returned by last peg-run+.")

(defun peg-group-data-clear (&rest r)
  (setq peg-group-data nil))
(advice-add 'peg-run :before 'peg-group-data-clear)

(defun peg-group-data (prop)
  (plist-get peg-group-data prop))

(defun peg-group-beg (prop)
  (nth 0 (peg-group-data prop)))

(defun peg-group-end (prop)
  (nth 1 (peg-group-data prop)))

(defun peg-group-string (prop)
  (nth 2 (peg-group-data prop)))

(define-peg-rule group (pex prop)
  (and `(-- (point)) (funcall pex) `(-- (point))
       `(start end --
               (let* ((string (buffer-substring start end))
                      (data (list start end string)))
                 (setq peg-group-data
                       (append peg-group-data
                               (list prop data)))))))

(define-peg-rule to (pex)
  ;; 匹配任意字符到满足 PEX 之前为止
  (* (and (not (funcall pex)) (any))))

(define-peg-rule to-eol ()
  (to (peg (eol))))

(define-peg-rule group-to (pex prop)
  ;; 从当前位置开始捕获，直到匹配 PEX 前为止
  (group (peg (to pex)) prop))

(define-peg-rule to-group (prop pex)
  ;; 从当前位置开始匹配，并捕获 pex
  (to pex) (group pex prop))

(define-peg-rule to-point (pos)
  ;; 匹配任意字符到 pos 位置为止
  (* (and (guard (< (point) pos)) (any))))

(define-peg-rule group-to-point (pos prop)
  ;; 匹配任意字符到 pos 位置为止
  (group (peg (to-point pos)) prop))

;; (define-peg-rule+ any-to-pex (pex)
;;   ;; 匹配不满足 PEX 的任意字符为止
;;   (* (and (not pex) (any))))

;; (define-peg-rule+ any-to-eol ()
;;   (any-to-pex (eol)))

;; (define-peg-rule+ any-to-group (pex)
;;   ;; 从当前位置开始匹配，并捕获 pex
;;   (any-to-pex pex) (group pex))

;; (define-peg-rule+ group-to-pex (pex)
;;   ;; 从当前位置开始捕获，直到匹配 pex 前为止
;;   (group (any-to-pex pex)))

;; ;; (define-peg-rule group-to-pex (pex)
;; ;;   ;; 从当前位置开始捕获，直到匹配 pex 前为止
;; ;;   (group (peg (any-to-pex pex))))

(provide 'peg-plus)
