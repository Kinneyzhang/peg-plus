;; -*- lexical-binding: t -*-

;; (require 'peg-macros)

;;; custom rule

(require 'peg)
(require 'peg-utils)

(with-eval-after-load 'peg
  (put 'define-peg-rule 'lisp-indent-function 'defun)
  (put 'define-peg-ruleset 'lisp-indent-function 'defun))

(defvar peg-group-data nil
  "A plist data returned by last peg-run+.")

(defun peg-group-data-clear (&rest r)
  (setq peg-group-data nil))
(advice-add 'peg-run :before 'peg-group-data-clear)

(defun peg-group--check-arg (arg)
  "检查 `peg-group-data' 类型，约束 `peg-group-data',
`peg-group-beg',`peg-group-end', `peg-group-string' 参数的类型。"
  (cond
   ((and (peg-plistp peg-group-data) (numberp arg))
    (error "prop-or-nth '%s' should be a keyword." arg))
   ((and (not (peg-plistp peg-group-data)) (keywordp arg))
    (error "prop-or-nth '%s' should be a number." arg))))

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
  (peg-group--check-arg prop-or-nth)
  (let ((prop-or-nth (or prop-or-nth 0)))
    (cond ((numberp prop-or-nth)
           (if (= prop-or-nth 0)
               peg-group-data ;; all data
             (nth (1- prop-or-nth) peg-group-data)))
          ((keywordp prop-or-nth)
           (plist-get peg-group-data prop-or-nth))
          (t (error "Invalid format of prop-or-nth: %S" prop-or-nth)))))

(defun peg-group-beg (&optional prop-or-nth)
  "Return the beginning point of group data filtered by PROP-OR-NTH."
  (peg-group--check-arg prop-or-nth)
  (let ((data (peg-group-data prop-or-nth)))
    (if (or (null prop-or-nth)
            (equal prop-or-nth 0))
        ;; all groups data
        (if (peg-plistp data)
            (--map (nth 0 it) (peg-plist-values data))
          (--map (nth 0 it) data))
      ;; specific group data
      (nth 0 data))))

(defun peg-group-end (&optional prop-or-nth)
  "Return the end point of group data filtered by PROP-OR-NTH."
  (peg-group--check-arg prop-or-nth)
  (let ((data (peg-group-data prop-or-nth)))
    (if (or (null prop-or-nth)
            (equal prop-or-nth 0))
        ;; all groups data
        (if (peg-plistp data)
            (--map (nth 1 it) (peg-plist-values data))
          (--map (nth 1 it) data))
      ;; specific group data
      (nth 1 data))))

(defun peg-group-string (&optional prop-or-nth)
  "Return the string of group data filtered by PROP-OR-NTH."
  (peg-group--check-arg prop-or-nth)
  (let ((data (peg-group-data prop-or-nth)))
    (if (or (null prop-or-nth)
            (equal prop-or-nth 0))
        ;; all groups data
        (if (peg-plistp data)
            (--map (nth 2 it) (peg-plist-values data))
          (--map (nth 2 it) data))
      ;; specific group data
      (nth 2 data))))

;; (defun peg-group-check-rule ()
;;   "检查在同一个 peg-run 的规则里，group 参数是否全部设置 prop
;; 全部不设置。不允许部分设置 keyword。"
;;   )

(define-peg-rule group (pex &optional prop)
  (and `(-- (point)) (funcall pex) `(-- (point))
       `(start end --
               (let* ((string (buffer-substring-no-properties start end))
                      (data (list start end string)))
                 (setq peg-group-data
                       (append peg-group-data
                               (if prop
                                   ;; plist
                                   (list prop data)
                                 ;; cons list
                                 (list data))))))))

(define-peg-rule before (pex)
  ;; match any chars before PEX.
  (or (and (+ (and (not (funcall pex)) (any)))
           (not (eob)))
      ;; if pex is (eob)
      (and (+ (and (not (funcall pex)) (any)))
           (eob) (funcall pex))))

(define-peg-rule until (pex)
  ;; match any chars until the end of PEX.
  (before pex) (funcall pex))

(define-peg-rule before-point (pex pos)
  ;; search PEX before point POS.
  (until pex) (to pos))

(define-peg-rule to (pos)
  ;; match any chars to point POS.
  (or (+ (and (guard (< (point) pos)) (any)))
      (and (guard (= (point) pos)))))

(define-peg-rule n (point n)
  ;; match N times any chars, POINT must be (point)
  (* (and (guard (< (point) (+ point n))) (any))))

(define-peg-rule group-before (pex &optional prop)
  ;; match any chars before pex and group.
  (group (peg (before pex)) prop))

(define-peg-rule group-until (pex &optional prop)
  ;; match any chars before pex and group.
  (group (peg (until pex)) prop))

(define-peg-rule group-pex (pex &optional prop)
  ;; 从当前位置开始匹配，并捕获 pex
  (before pex) (group pex prop))

(define-peg-rule group-to (pos &optional prop)
  ;; 匹配任意字符到 pos 位置为止
  (group (peg (to pos)) prop))

(define-peg-rule group-all-pex (pex &optional prop)
  ;; 从当前位置开始匹配，并捕获 pex
  (* (group-pex pex prop)))

(provide 'peg-plus)
