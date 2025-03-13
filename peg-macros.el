;; -*- lexical-binding: t -*-

(require 'peg)
(require 'dash)

;;; FIXME: 处理参数名可能和规则同名的问题

(defun peg-rule-action-p (name)
  (eq name (intern "`")))

(defun peg-rule-function-name (name)
  (intern (concat "peg-rule " (symbol-name name))))

(defun peg-rule-customed-p ()
  "判断规则 NAME 是否为自定义规则"
  )

(defun peg-rule-built-in-p (name)
  "判断规则 NAME 是否为内置规则"
  (not (functionp (peg-rule-function-name name))))

(functionp (peg-rule-function-name 'not))

(defun peg-rule-costom-p (name)
  "判断规则 NAME 是否为自定义规则"
  (functionp (peg-rule-function-name name)))

(defun peg-rule-args (name)
  "返回规则 NAME 的参数列表"
  (when (symbolp name)
    (let* ((func (peg-rule-function-name name))
           (args (help-function-arglist func)))
      (when (and (functionp func)
                 (consp args))
        args))))

(defun peg-rule-body (name)
  "返回规则 NAME 的规则定义"
  (or (function-get (peg-rule-function-name name) :peg-body)
      (error "PEG rule '%s' is not defined by `define-peg-rule+'"
             name)))

(defun peg--map-pex (pred fun pex)
  (when (funcall pred pex)
    (setq pex (funcall fun pex)))
  (when (and (consp pex) (consp (cdr pex)))
    (setq pex (-map (lambda (x)

                      (peg--map-pex pred fun x))
                    pex)))
  pex)

;;; wrap rule arg with peg if it's a peg matcher

(defmacro peg-map-pexs (pred form pexs)
  "Anaphoric form of `peg--map-pexs'."
  (declare (debug (def-form def-form form)))
  `(--map
    (peg--map-pex (lambda (it) (ignore it) ,pred)
                  (lambda (it) (ignore it) ,form)
                  it)
    ,pexs))

(defun peg--rule-arg-plist (name)
  "根据参数列表处理规则的定义，返回一个 plist。
:pex-args 是这个规则用于组成 pex 的参数列表；
:exp-args 是用于 elisp 表达式中的参数列表(用于 guard 或 action 中)
:new-pexs 是给需要的参数包裹了 funcall 的新的 pexs 表达式。"
  (let* ((macro-args (peg-rule-args name))
         (macro-pexs (peg-rule-body name))
         (_ (progn
              (elog-info "macro-args:" macro-args)
              (elog-info "macro-pexs:" macro-pexs)
              t))
         all-pex-args curr-pex-args rule-name rule-params
         (new-pexs
          (--tree-map-nodes
           (when-let* ((_ (and (consp it)
                               (not (peg-rule-action-p it))))
                       (name (setq rule-name (car it)))
                       (_ (progn
                            (elog-info "it:" it)
                            (elog-info "name:" name)
                            t))
                       (params (setq rule-params (cdr it)))
                       ;; 判断规则名
                       ;; (_ (and (peg-rule-built-in-p name)
                       ;;         (not (eq name 'guard))
                       ;;         (not (eq name 'funcall))))
                       ;; 参数至少有一个属于 macro-args
                       (_ (setq curr-pex-args
                                (--filter (member it params)
                                          macro-args))))
             t)
           (progn
             (elog-info "curr-pex-args:" curr-pex-args)
             (cons rule-name
                   (--map (if (and (peg-rule-built-in-p name)
                                   (not (eq name 'guard))
                                   (not (eq name 'funcall))
                                   (member it curr-pex-args))
                              (progn
                                (add-to-list 'all-pex-args it)
                                (list 'funcall it))
                            it)
                          rule-params)))
           macro-pexs)))
    `( :pex-args ,all-pex-args
       :exp-args ,(list-substract macro-args all-pex-args)
       :new-pexs ,new-pexs)))

(defun peg--rule-args-add-peg (pexs &optional macro-args)
  (let (pex-args rule-name rule-params)
    (peg-map-pexs
     (when-let* ((_ (consp it))
                 (name (setq rule-name (car it)))
                 ;; params 是 peg-run 中规则实际使用的参数
                 ;; 没有参数则不用处理
                 (params (setq rule-params (cdr it)))
                 (_ (not (eq name (intern "`"))))
                 ;; 只有自定义的规则的参数才可能需要 peg 包裹
                 (_ (peg-rule-costom-p name))
                 ;; 只有 pex 格式的参数才需要 peg 包裹
                 (plist (peg--rule-arg-plist name))
                 (args (plist-get plist :pex-args))
                 ;; 是pex格式的参数 且 不属于宏参数时，使用 peg 包裹
                 (_ (if macro-args
                        ;; 在使用 define-peg-rule+ 定义规则时，
                        ;; pex参数全部属于宏参数，无需包裹 peg
                        ;; 不属于宏参数的部分，包裹 peg
                        (setq pex-args (-difference args macro-args))
                      ;; 在 peg+ 中，所有 pex 都需要包裹 peg
                      t)))
       t)
     (cons rule-name
           ;; 获取 pex-args 在所有参数中的位置，
           ;; 并给 rule-params 中对应位置的参数加上 peg
           (let ((indexs
                  (--map (-elem-index it (peg-rule-args rule-name))
                         pex-args)))
             (--map-indexed
              (if (and (member it-index indexs))
                  (list 'peg it)
                it)
              rule-params)))
     pexs)))

;; 这三个宏要配合使用:
;; peg+ 和 peg-run+ 中用到的 自定义规则，必须是由 define-peg-rule+ 定义的
;; 因为在判断参数是否为 exp 时，需要使用 (peg--rule-arg-plist args pexs) 函数
;; 该函数的 pexs 为规则定义表达式，在运行 peg-run+ 时需要通过规则的名称来获取
;; 不允许透传参数

;; FIXME: 排除 action 列表的处理
(defmacro define-peg-rule+ (name args &rest pexs)
  ;; define-peg-rule 如果有参数，参数使用时用 funcall 调用；
  ;; 如果自定义的规则有参数，在所有参数加上 peg 生成 peg-matcher
  (declare (indent defun))
  (function-put (peg-rule-function-name name) :peg-body pexs)
  (let* ((new-pexs (peg--rule-args-add-peg new-pexs args)))
    `(define-peg-rule ,name ,args
       ,@new-pexs)))

(defmacro peg+ (&rest pexs)
  `(peg ,@(peg--rule-args-add-peg pexs)))

(defmacro peg-run+ (&rest pexs)
  `(peg-run (peg+ ,@pexs)))

(provide 'peg-macros)
