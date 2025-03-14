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
