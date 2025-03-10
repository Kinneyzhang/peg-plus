;; -*- lexical-binding: t -*-

(require 'ert)
(require 'peg-plus)

(defun peg-test-before ()
  (should
   (equal
    15 (with-temp-buffer
         (insert "happy hacking emacs and vim and vscode.")
         (goto-char (point-min))
         (peg-run (peg (before (peg "emacs"))))
         (point)))))

(defun peg-test-until ()
  (should
   (equal
    20 (with-temp-buffer
         (insert "happy hacking emacs and vim and vscode.")
         (goto-char (point-min))
         (peg-run (peg (until (peg "emacs"))))
         (point)))))

(defun peg-test-to ()
  (should
   (equal
    37 (with-temp-buffer
         (insert "happy hacking emacs and vim and vscode.")
         (goto-char (point-min))
         (peg-run (peg (to 37)))
         (point)))))

(defun peg-test-n ()
  (should
   (equal
    14 (with-temp-buffer
         (insert "happy hacking emacs and vim and vscode.")
         (goto-char (point-min))
         (peg-run (peg (n (point) 13)))
         (point)))))

(defun peg-test-group ()
  (should
   (equal
    '(;; all groups
      (((1 15 "happy hacking ") (25 40 "vim and vscode."))
       ((1 15 "happy hacking ") (25 40 "vim and vscode."))
       (1 25) (1 25)
       (15 40) (15 40) 
       ("happy hacking " "vim and vscode.")
       ("happy hacking " "vim and vscode."))
      ;; group1
      ((1 15 "happy hacking ")
       1 15 "happy hacking ")
      ;; group2
      ((25 40 "vim and vscode.")
       25 40 "vim and vscode."))
    (with-temp-buffer
      (insert "happy hacking emacs and vim and vscode.")
      (goto-char (point-min))
      (peg-run (peg (group (peg "happy hacking "))
                    (before (peg "vim"))
                    (group (peg (to (line-end-position))))))
      (list
       ;; all groups
       (list (peg-group-data) (peg-group-data 0)
             (peg-group-beg) (peg-group-beg 0)
             (peg-group-end) (peg-group-end 0)
             (peg-group-string) (peg-group-string 0))
       ;; group1
       (list (peg-group-data 1)
             (peg-group-beg 1) (peg-group-end 1) (peg-group-string 1))
       ;; group2
       (list (peg-group-data 2)
             (peg-group-beg 2) (peg-group-end 2) (peg-group-string 2)))))))

(defun peg-test-group-prop ()
  (should
   (equal
    '(;; all groups
      ((:g1 (1 15 "happy hacking ") :g2 (25 40 "vim and vscode."))
       (1 25) (15 40) ("happy hacking " "vim and vscode."))
      ;; g1 group
      ((1 15 "happy hacking ")
       1 15 "happy hacking ")
      ;; g2 group
      ((25 40 "vim and vscode.")
       25 40 "vim and vscode."))
    (with-temp-buffer
      (insert "happy hacking emacs and vim and vscode.")
      (goto-char (point-min))
      (peg-run (peg (group (peg "happy hacking ") :g1)
                    (before (peg "vim"))
                    (group (peg (to (line-end-position))) :g2)))
      (list
       ;; all groups
       (list (peg-group-data)
             (peg-group-beg) (peg-group-end) (peg-group-string))
       ;; g1 group
       (list (peg-group-data :g1)
             (peg-group-beg :g1) (peg-group-end :g1) (peg-group-string :g1))
       ;; g2 group
       (list (peg-group-data :g2)
             (peg-group-beg :g2) (peg-group-end :g2) (peg-group-string :g2)))))))

(defun peg-test-group-before ()
  (should
   (equal
    '(((1 15 "happy hacking ") (15 25 "emacs and "))
      (1 15) (15 25) ("happy hacking " "emacs and "))
    (with-temp-buffer
      (insert "happy hacking emacs and vim and vscode.")
      (goto-char (point-min))
      (peg-run (peg (group-before (peg "emacs"))
                    (group-before (peg "vim"))
                    (until (peg (eol)))))
      ;; all groups
      (list (peg-group-data)
            (peg-group-beg) (peg-group-end) (peg-group-string))))))

(defun peg-test-group-until ()
  (should
   (equal
    '(((1 20 "happy hacking emacs") (20 28 " and vim"))
      (1 20) (20 28) ("happy hacking emacs" " and vim"))
    (with-temp-buffer
      (insert "happy hacking emacs and vim and vscode.")
      (goto-char (point-min))
      (peg-run (peg (group-until (peg "emacs"))
                    (group-until (peg "vim"))
                    (to (point-max))))
      ;; all groups
      (list (peg-group-data)
            (peg-group-beg) (peg-group-end) (peg-group-string))))))

(defun peg-test-group-pex ()
  (should
   (equal
    '(((15 20 "emacs") (25 28 "vim"))
      (15 25) (20 28) ("emacs" "vim"))
    (with-temp-buffer
      (insert "happy hacking emacs and vim and vscode.")
      (goto-char (point-min))
      (peg-run (peg (group-pex (peg "emacs"))
                    (group-pex (peg "vim"))
                    (to (point-max))))
      ;; all groups
      (list (peg-group-data)
            (peg-group-beg) (peg-group-end) (peg-group-string))))))

;; (defun peg-test-group-to ()
;;   (should
;;    (equal
;;     '(((15 20 "emacs") (25 28 "vim"))
;;       (15 25) (20 28) ("emacs" "vim"))
;;     (with-temp-buffer
;;       (insert "happy hacking emacs and vim and vscode.")
;;       (goto-char (point-min))
;;       (peg-run (peg (group-to 14)
;;                     (group-to 25)
;;                     (to (point-max))))
;;       ;; all groups
;;       (list (peg-group-data)
;;             (peg-group-beg) (peg-group-end) (peg-group-string))))))

(defun peg-test-group-pex-to ()
  (should
   (equal
    '((15 20 "emacs") 15 20 "emacs")
    (with-temp-buffer
      (insert "happy hacking emacs and vim and vscode.")
      (goto-char (point-min))
      (peg-run (peg (group-pex-to (peg "emacs") 25)))
      ;; all groups
      (list (peg-group-data 1)
            (peg-group-beg 1) (peg-group-end 1) (peg-group-string 1))))))

(progn
  (peg-test-before)
  (peg-test-until)
  (peg-test-to)
  (peg-test-group)
  (peg-test-group-prop)
  (peg-test-group-before)
  (peg-test-group-until)
  (peg-test-group-pex)
  (peg-test-group-pex-to)
  (peg-test-n))

(provide 'peg-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro peg-search-forward (pex &optional limit)
  "从当前位置到 LIMIT 内搜索 PEX。
如果能搜索到 PEX，返回 (start . end)，光标移动到 PEX 后面；
如果匹配不到，返回 nil，光标移动到 PEX 后面。"
  (declare (indent defun))
  (let ((limit (if limit
                   (if (consp limit)
                       (eval limit)
                     limit)
                 (point-max))))
    (unless (< limit (point))
      `(progn
         (peg-run (peg (group-pex-to (peg ,pex) ,limit)))
         (goto-char ,limit)
         (peg-group-data)))))

;; 返回所有6500前的 pex 匹配
(peg-search-forward (and "emacs" [0-9]) 7010)

(with-current-buffer "test-peg"
  (save-excursion
    (goto-char (point-min))
    (with-consume-time "peg"
      (peg-search-forward "emacs")
      )))

(with-current-buffer "test-peg"
  (save-excursion
    (goto-char (point-min))
    (with-consume-time "regexp"
      (while (re-search-forward "emacs" nil t)))))

(with-temp-buffer
  (insert "(a (b (c) (d)))")
  (goto-char (point-min))
  (with-peg-rules ((pairs "("  ")")
                   (pair "(" (* non-parens)  ")")
                   (non-parens (and (not parens) (any)))
                   (parens (or "(" ")")))
    (peg-run (peg pair))
    ;; (point)
    ))

;; (peg-run (peg (group-pex-to (peg (and "emacs" [0-9])) 6888)))

;; ;; test group all pex before point
;; (peg-run (peg (+ (and (group-pex (peg (and "emacs" [0-9])))
;;                       (and (guard (< (point) 6758))))
;;                  )
;;               ))

;; (peg-run (peg (group-all-pex (peg (and "emacs" [0-9])))
;;               (to 6500)))

;; (group-pex-to )

;; emacs1

;; (peg-run (peg
;;           (and (group-pex
;;                 (peg (and "emacs" [0-9] [0-9])))
;;                (to 1))))
;; emacs21
;; (progn
;;   (peg-run (peg (to 5311)))
;;   (point))

;; (peg-run (peg (until (peg "emacsc"))))
;; (peg-run (peg (to 611)))
;; (peg-run (peg (before-point (peg "emacsx") 6185)))
;; (point)
;; emacsx

;; (progn
;;   (peg-search-string
;;    "emacs"
;;    "happy hacking emacs1 and emacs is a lifestyle.")
;;   (peg-group-data))

;; (eval
;;  (macroexpand-all
;;   '(peg-run (peg (* (group-pex (peg "emacs")))))))

;; ;; vnioewrbivw emacs21

;; (eval-expression
;;  '(peg-run (peg (* (group-pex (peg "emacs"))))))
