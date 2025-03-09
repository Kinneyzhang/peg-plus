;; -*- lexical-binding: t -*-

;; (require 'peg-macros)

;;; custom rule

(require 'peg)

(defvar peg-group-data nil
  "A plist data returned by last peg-run+.")

(defun peg-group-data-clear (&rest r)
  (setq peg-group-data nil))
(advice-add 'peg-run :before 'peg-group-data-clear)

(defun peg-group-data (&optional prop-or-nth)
  "PROP-OR-NTH is nil, keyword or number. This function
return the value of `peg-group-data' filtered by PROP-OR-NTH.

The variable `peg-group-data' stores data of all groups.
The variable has two types of value: a list of cons or a plist.

For 'a list of cons', PROP-OR-NTH should be a number or nil,
which means the number(starts from 1) order of data in groups
 or all data. If the number is zore, it also returns all data.

For plist, PROP-OR-NTH should be a keyword to get the value of
it in group."
  (let ((prop-or-nth (or prop-or-nth 0)))
    (cond ((numberp prop-or-nth)
           (if (= prop-or-nth 0)
               peg-group-data ;; all data
             (nth (1- prop-or-nth) peg-group-data)))
          ((keywordp prop-or-nth)
           (plist-get peg-group-data prop-or-nth))
          (t (error "Invalid format of prop-or-nth: %S" prop-or-nth)))))

;; (:x (1 15 "happy hacking ") :y (15 60 "emacs, vim and vscode.
;; happy hacking emacs1, ") :z (60 77 "vim1 and vscode1."))

;; ((1 15 "happy hacking ") (15 60 "emacs, vim and vscode.
;; happy hacking emacs1, ") (60 77 "vim1 and vscode1."))


(defun peg-group-beg (&optional prop-or-nth)
  "Return the beginning point of group data filtered by PROP-OR-NTH."
  )

(defun peg-group-end (&optional prop-or-nth)
  )

(defun peg-group-string (&optional prop-or-nth)
  )

(define-peg-rule group (pex &optional prop)
  (and `(-- (point)) (funcall pex) `(-- (point))
       `(start end --
               (let* ((string (buffer-substring start end))
                      (data (list start end string)))
                 (setq peg-group-data
                       (append peg-group-data
                               (if prop
                                   ;; plist
                                   (list prop data)
                                 ;; cons list
                                 (list data))))))))

(define-peg-rule to (pex)
  ;; 匹配任意字符到满足 PEX 之前为止
  (* (and (not (funcall pex)) (any))))

(define-peg-rule to-eol ()
  (to (peg (eol))))

(define-peg-rule group-to (pex &optional prop)
  ;; 从当前位置开始捕获，直到匹配 PEX 前为止
  (group (peg (to pex)) prop))

(define-peg-rule to-group (pex &optional prop)
  ;; 从当前位置开始匹配，并捕获 pex
  (to pex) (group pex prop))

(define-peg-rule to-point (pos)
  ;; 匹配任意字符到 pos 位置为止
  (* (and (guard (< (point) pos)) (any))))

(define-peg-rule group-to-point (pos &optional prop)
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
