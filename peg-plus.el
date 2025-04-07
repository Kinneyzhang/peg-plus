;; -*- lexical-binding: t -*-

(require 'peg)
;; (require 'peg-macros)
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

;; (cl-defmethod peg--translate ((_ (eql any)) )
;;   '(when (not (eolp))
;;      (forward-char)
;;      t))

(peg--translate 'any)
(peg--translate 'and '(str "emacs") '(str "vim"))
(peg-translate-exp '(and (str "emacs") (str "vim")))

peg-leaf-types

(t-ps (and (+ (any)) (if "emacs"))
      "happy hacking emacs")

(peg-run
 (save-excursion
   (or (and (or (when (looking-at '"emacs")
                  (goto-char (match-end 0)) t)
                (peg--record-failure '(str "emacs")))
            (or (when (looking-at '"vim")
                  (goto-char (match-end 0)) t)
                (peg--record-failure '(str "vim"))))
       (peg--record-failure '(and (str "emacs") (str "vim"))))))

emacsvim

(peg-run (peg (+ (any)) "emacs"))

(with-temp-buffer
  ;; (insert "happy hacking emacs and vscode!")
  (insert "happy hacking vim and vscode!
happy hacking emacs and vscode!")
  (goto-char (point-min))
  (peg-run (peg (before (peg "emacs"))))
  ;; (point)
  )

(define-peg-rule before (pex)
  ;; 1.当前光标位置为 pex，不移动光标，匹配成功
  ;; 2.光标后没有匹配的 pex, 不移动光标，匹配失败
  ;; 3.光标后面有匹配的 pex, 移动光标到 pex之前，匹配成功
  ;; 需要一个规则判断后续的是否有匹配的，但不移动光标
  (or
   (has (funcall pex)) ;; extend to if, do not move cursor if failed
   (if (funcall pex))
   (and (+ (and (not (funcall pex)) (any)))
        (not (eob)))
   ;; if pex is (eob)
   (and (+ (and (not (funcall pex)) (any)))
        (eob) (funcall pex))))

(peg-run (peg (before (peg "emacs"))))
(peg-run (peg (if (and (* (not "emacs") (any))
                       "emacs"))))

(peg-run (peg (if (+ (and (not "emacs") (any))))))

(peg-run (peg (if (and (+ (and (not "emacs") (any)))
                       (if "emacs")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro t-ps (pex string)
  `(with-temp-buffer
     (save-excursion (insert ,string))
     (peg-run (peg (and (bob) ,pex (eob))))))

(define-peg-rule Qu (rule m n)
  (guard
   (let ((cnt 0))
     (while (and (< cnt n) (funcall rule))
       (cl-incf cnt))
     (if (<= m cnt n) t nil))))

(t-ps (Qu (peg " ") 3 5) "      ") ;;=> nil
(t-ps (Qu (peg " ") 1 2) " ") ;;=> t
(t-ps (Qu (peg " ") 1 2) "  ") ;;=> t
(t-ps (Qu (peg " ") 1 2) "   ") ;;=> nil

(define-peg-rule loop (rule n)
  (guard
   (let ((cnt 0))
     (while (and (< cnt n) (funcall rule))
       (cl-incf cnt))
     (if (>= cnt n) t nil))))

(peg-run (peg (loop (peg "emacs") 2)))

emacsemacsemacsemacs

;; (cl-defmethod peg--translate ((_ (eql   any)) &optional n)
;;   `(when (not (eobp))
;;      (forward-char ,(or n 1))
;;      t))

(define-peg-rule group-before (pex &optional prop)
  ;; match any chars before pex and group.
  (group (peg (before pex)) prop))

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
