(defun peg-plistp (obj)
  "严格检查 OBJ 是否为 plist，且所有键均为关键字。"
  (and (plistp obj)
       (catch 'invalid
         (while obj
           (unless (keywordp (car obj))
             (throw 'invalid nil))
           (setq obj (cdr (cdr obj))))
         t)))

(defun peg-plist-values (plist)
  (let ((lst))
    (while plist
      (pop plist)
      (push (pop plist) lst))
    (nreverse lst)))
