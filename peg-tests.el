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
         (peg-run (peg (any-to 37)))
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
                    (group (peg (any-to (line-end-position))))))
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
                    (group (peg (any-to (line-end-position))) :g2)))
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

(defun peg-test-match ()
  (should
   (equal
    '(((15 20 "emacs") (25 28 "vim"))
      (15 25) (20 28) ("emacs" "vim"))
    (with-temp-buffer
      (insert "happy hacking emacs and vim and vscode.")
      (goto-char (point-min))
      (peg-run (peg (match (peg "emacs"))
                    (match (peg "vim"))
                    (any-to (point-max))))
      ;; all groups
      (list (peg-group-data)
            (peg-group-beg) (peg-group-end) (peg-group-string))))))

(progn
  (peg-test-before)
  (peg-test-until)
  (peg-test-to)
  (peg-test-group)
  (peg-test-group-prop)
  (peg-test-match))

;; (with-consume-time "pex"
;;   (with-current-buffer "*elog*"
;;     (goto-char (point-min))
;;     (peg-search (pair (peg "(") (peg ")")))))

;; (length peg-group-data)

(provide 'peg-tests)
