(require 'ert)
(require 'peg)

(defmacro peg-parse-string (pex string &optional noerror)
  "Parse STRING according to PEX.
If NOERROR is non-nil, push nil resp. t if the parse failed
resp. succeeded instead of signaling an error."
  (declare (indent 1))
  (let ((oldstyle (consp (car-safe pex)))) ;PEX is really a list of rules.
    `(with-temp-buffer
       (insert ,string)
       (goto-char (point-min))
       ,(if oldstyle
            `(with-peg-rules ,pex
               (peg-run (peg ,(caar pex))
                        ,(unless noerror '#'peg-signal-failure)))
          `(peg-run (peg ,pex)
                    ,(unless noerror '#'peg-signal-failure))))))

(ert-deftest peg-test ()
  )

(defmacro peg (&rest pexs)
  "Return a PEG-matcher that matches PEXS."
  (pcase (peg-normalize `(and . ,pexs))
    (`(call ,name) `#',(peg--rule-id name)) ;Optimize this case by η-reduction!
    (exp `(peg--lambda ',pexs () ,(peg-translate-exp exp)))))
