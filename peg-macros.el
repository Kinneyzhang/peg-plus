;; -*- lexical-binding: t -*-

(require 'peg)
(require 'dash)

;;; FIXME: 处理参数名可能和规则同名的问题

(defun peg-rule-full-name (name)
  (intern (concat "peg-rule " (symbol-name name))))

(defun peg--custom-rule-arglist (name)
  "是自定义规则，且有参数，返回参数列表"
  (when (symbolp name)
    (let* ((rule-func (peg-rule-full-name name))
           (arglist (help-function-arglist rule-func)))
      (when (and (functionp rule-func)
                 (consp arglist))
        arglist))))

(defun tree-has-elem-p (tree elem)
  (let ((has-elem nil))
    (--tree-map (when (eq it elem)
                  (setq has-elem t))
                tree)
    has-elem))

(defun peg--rule-has-guard-p (pexs)
  "判断 PEXS 中是否有 guard"
  (tree-has-elem-p pexs 'guard))

(defun peg--rule-guard-exps (pexs)
  "获取 PEXS 中所有 guard 规则的表达式列表"
  (let (guard-exps)
    (--tree-map-nodes
     (and (consp it)
          (eq (car it) 'guard))
     (push (cadr it) guard-exps)
     pexs)
    guard-exps))

(defun peg--rule-arg-in-guard-p (arg pexs)
  "判断参数 ARG 是否在 PEXS 的 guard 规则内"
  (let ((exps (peg--rule-guard-exps pexs)))
    (tree-has-elem-p exps arg)))

(defun peg--rule-arg-plist (args pexs)
  "根据参数是否在 guard 里面分类参数，形成 plist"
  (let (pex-args exp-args)
    ;; 判断 PEXS 中是否有 guard，没有则参数属于 pex-args
    ;; 否则，参数在 guard 内使用的属于 exp-args
    ;; pex-args 需要使用 funcall 调用
    ;; exp-args 无需使用 funcall 调用
    (if (peg--rule-has-guard-p pexs)
        (progn
          (dolist (arg args)
            (if (peg--rule-arg-in-guard-p arg pexs)
                (push arg exp-args)
              (push arg pex-args)))
          (list :pex-args pex-args :exp-args exp-args))
      (list :pex-args args :exp-args nil))))

(defun peg--rule-tree-depth (tree)
  (cond
   ((not (listp tree)) 0)
   ((null tree) 1)
   (t (1+ (apply #'max 0 (mapcar #'peg--rule-tree-depth tree))))))

(defun peg--rule-args-add-peg-1 (pexs)
  (let (pex-args all-args name params)
    (--tree-map-nodes
     (when-let* ((_ (consp it))
                 (rule-name (setq name (car it)))
                 ;; rule-params 是 peg-run 中规则实际使用的参数
                 (rule-params (setq params (cdr it)))
                 ;; args 是规则定义时的形式参数
                 (args (setq all-args
                             (peg--custom-rule-arglist rule-name)))
                 ;; here is rule pexs, not peg-run pexs
                 (plist (peg--rule-arg-plist
                         ;; 获取 define-peg-rule+ 中存放的函数体
                         args (function-get (peg-rule-full-name name)
                                            :body))))
       ;; 考虑自定义规则有多个参数的情况：
       ;; 如果 args 中至少有一个 pex arg (需要加 peg)
       ;; 并且 没有一个参数已经加上了 peg，则满足条件，给需要加的参数加上peg
       ;; 否则其他情况，都不满足条件
       (and (setq pex-args (plist-get plist :pex-args))
            (--none? (when (consp it) (eq (car it) 'peg))
                     rule-params)))
     (cons name
           ;; 给属于 pex-args 的 arg 加上 peg
           ;; 获取 pex-args 在所有参数中的位置，
           ;; 并给 rule-params 中对应位置的参数加上 peg
           (let ((indexs (--map (-elem-index it all-args)
                                pex-args)))
             (--map-indexed
              (if (member it-index indexs)
                  (list 'peg it)
                it)
              params)))
     pexs)))

(defun peg--rule-args-add-peg (pexs)
  (let ((depth (peg--rule-tree-depth pexs)))
    (dotimes (i (1- depth))
      (setq pexs (peg--rule-args-add-peg-1 pexs)))
    pexs))

(defun peg--rule-add-funcall (args pexs)
  "pex 规则的参数才使用 funcall 调用，guard 的 elisp 表达式参数保持原样"
  (let* ((plist (peg--rule-arg-plist args pexs))
         (pex-args (plist-get plist :pex-args)))
    (if pex-args
        (--tree-map
         (if (member it pex-args)
             (list 'funcall it)
           it)
         pexs)
      pexs)))

;; 这三个宏要配合使用:
;; peg+ 和 peg-run+ 中用到的 自定义规则，必须是由 define-peg-rule+ 定义的
;; 因为在判断参数是否为 exp 时，需要使用 (peg--rule-arg-plist args pexs) 函数
;; 该函数的 pexs 为规则定义表达式，在运行 peg-run+ 时需要通过规则的名称来获取

(defmacro define-peg-rule+ (name args &rest pexs)
  ;; define-peg-rule 如果有参数，参数使用时用 funcall 调用；
  ;; 如果自定义的规则有参数，在所有参数加上 peg 生成 peg-matcher
  (declare (indent defun))
  (let ((new-pexs (peg--rule-add-funcall
                   args (peg--rule-args-add-peg pexs)))
        (peg-func (peg-rule-full-name name)))
    (function-put peg-func :body new-pexs)
    `(define-peg-rule ,name ,args
       ,@new-pexs)))

(defmacro peg+ (&rest pexs)
  `(peg ,@(peg--rule-args-add-peg pexs)))

(defmacro peg-run+ (&rest pexs)
  `(peg-run (peg+ ,@pexs)))

(provide 'peg-macros)
