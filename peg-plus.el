;; -*- lexical-binding: t -*-

(require 'peg)
(require 'peg-utils)

(with-eval-after-load 'peg
  (put 'define-peg-rule 'lisp-indent-function 'defun)
  (put 'define-peg-ruleset 'lisp-indent-function 'defun))

;;; peg group

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

(define-peg-rule group (pex &optional prop)
  ;; group PEX and set match data in `peg-group-data'
  ;; if PROP is non-nil, get match data by PROP, otherwise by number
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

;;; peg group ends here

;;; other customized peg rules

(define-peg-rule pair (left right)
  ;; match recursive structure
  (funcall left)
  (* (or (and (if (funcall left))
              (pair left right))
	     (and (not (funcall right)) (any))))
  (funcall right))

(define-peg-rule loop (pex n)
  ;; match N pex
  (guard
   (let ((cnt 0))
     (while (and (< cnt n) (funcall pex))
       (cl-incf cnt))
     (if (>= cnt n) t nil))))

(define-peg-rule between (pex m n)
  ;; match the number of pex between M and N.
  (guard
   (let ((cnt 0))
     (while (and (< cnt n) (funcall pex))
       (cl-incf cnt))
     (if (<= m cnt n) t nil))))

(define-peg-rule before (pex)
  ;; match any chars before the start of PEX.
  (and (* (and (not (funcall pex)) (any)))        
       (if (funcall pex))))

(define-peg-rule until (pex)
  ;; match any chars until the end of PEX.
  (before pex) (funcall pex))

(define-peg-rule match (pex)
  ;; match PEX and set match data, like `re-search-forward'
  (before pex) (group pex))

(define-peg-rule any-to (pos)
  ;; match any chars to point POS.
  (* (and (guard (< (point) pos)) (any)))
  (guard (= (point) pos)))

;;; other customized peg rules ends here

(defun peg-eval (&optional pex)
  "Eval peg expression from minibuffer."
  (interactive "sEval PEX: ")
  (eval-expression
   `(peg-run+ ,(car (read-from-string pex)))))

(defmacro peg-string (string pex)
  "Return if STRING match PEX."
  `(with-temp-buffer
     (save-excursion (insert ,string))
     (peg-run (peg (and (bob) ,pex (eob))))))

(defmacro peg-run+ (&rest pexs)
  "Like peg-run, but do not move cursor if search failed."
  `(let ((start (point)))
     (if (peg-run (peg ,@pexs))
         t
       (goto-char start)
       nil)))

(defmacro peg-search-forward (pex &optional limit)
  "Search PEX in current buffer before point LIMIT,
and set the first matched data.
The matched data are stored in `peg-group-data'."
  `(let ((limit (or ,limit (point-max))))
     (peg-run+ (match (peg ,pex))
               (if (any-to limit)))))

(defmacro peg-search (pex &optional limit)
  "Search all PEXs before point LIMIT and set all matched data.
The matched data are stored in `peg-group-data'."
  `(let ((limit (or ,limit (point-max)))
         data)
     (while (peg-search-forward ,pex limit)
       (setq data (append data peg-group-data)))
     (setq peg-group-data data)
     (and data t)))

(defmacro with-peg-search (pex limit &rest body)
  "The same to `peg-search', but eval BODY when matched."
  `(let ((limit (or ,limit (point-max)))
         data)
     (while (peg-search-forward ,pex limit)
       ,@body
       (setq data (append data peg-group-data)))
     (setq peg-group-data data)
     (and data t)))

(provide 'peg-plus)
