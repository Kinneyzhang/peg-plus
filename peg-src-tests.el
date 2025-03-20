(require 'peg)

(peg--when-fboundp oclosure-define
  (oclosure-define peg-function
    "Parsing function built from PEG rule."
    pexs)

  (cl-defmethod cl-print-object ((peg peg-function) stream)
    (princ "#f<peg " stream)
    (let ((args (help-function-arglist peg 'preserve-names)))
      (if args
          (prin1 args stream)
        (princ "()" stream)))
    (princ " " stream)
    (prin1 (peg-function--pexs peg) stream)
    (princ ">" stream)))

(fboundp 'oclosure-define)
(oclosure-define )
(macroexp-progn body)

(documentation 'oclosure-define)


(cl-defmethod cl-print-object ((peg peg-function) stream)
  (princ "#f<peg " stream)
  (help-function-arglist peg 'preserve-names)
  (let ((args ))
    (if args
        (prin1 args stream)
      (princ "()" stream)))
  )

oclosure-lambda 和 普通的 lambda的区别，什么时候使用


(peg-function (pexs (or "1" "2")))
