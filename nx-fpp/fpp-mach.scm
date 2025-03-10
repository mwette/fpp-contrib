;;; fpp-mach.scm

;; Copyright (C) 2025 Matthew Wette
;; SPDX-License-Identifier: Apache-2.0

;; syntax: https://github.com/nasa/fpp/blob/main/
;;                compiler/lib/src/main/scala/syntax/Parser.scala

(define-module (fpp-mach)
  #:export (fpp-spec
            fpp-mach
            gen-fpp-files
            )
  #:use-module (nyacc lang util)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (ice-9 pretty-print)
  )

(define fpp-spec
  (lalr-spec
   (notice (string-append "Copyright 2025 Matthew Wette\n"
                          "SPDX-License-Identifier: Apache-2.0"))
   ;;(prec< '$ident 'dotted-id) couldn't get this to work
   (expect 1)
   (start translation-unit)
   (grammar

    (elt-sep (",") ("\n") (elt-sep "\n"))
    (mem-sep (";") ("\n") (mem-sep "\n"))

    (include-spec ("include" string ($$ `(include ,$2))))

    (translation-unit
     (module-mem-seq ($$ `(trans-unit ,(tl->list $1)))))

    (module-mem-seq
     ($empty ($$ (make-tl 'module-mem-set)))
     (mod-mem mem-sep module-mem-seq ($$ (tl-insert $3 $1))))
    (mod-mem
     (include-spec)
     (component-defn)
     (comp-inst-defn)
     (topology-defn)
     (const-defn)
     (module-defn)
     (port-defn)
     (struct-defn)
     (loc-spec)
     (abs-type-defn)
     (array-defn)
     (enum-defn)
     ;;(stmach-defn)
     )

    (module-defn
     ("module" ident "{" module-mem-seq "}"
      ($$ `(module-defn ,$2 ,(tl->list $4)))))

    ;; === const, abs-type, array, enum, struct, port defn's
    
    (const-defn ("constant" ident "=" expr ($$ `(const-defn ,$2 ,$4))))
    (abs-type-defn ("type" ident ($$ `(type-defn ,$2))))
    (array-defn ("array" ident "=" index ident ($$ `(array-defn ,$3 ,$2))))
    
    (enum-defn (enum-defn-3 ($$ (reverse $1))))
    (enum-defn-0 ("enum" ident ($$ (list $2 'enum-defn))))
    (enum-defn-1 (enum-defn-0)
                 (enum-defn-0 ":" type-name ($$ (cons $3 $1))))
    (enum-defn-2 (enum-defn-1 "{" enum-mem-seq "}" ($$ (cons (tl->list $3) $1))))
    (enum-defn-3 (enum-defn-2)
                 (enum-defn-2 "default" expr ($$ (cons `(default ,$3) $1))))
    (enum-mem-seq
     ($empty ($$ (make-tl 'enum-mem-seq)))
     (enum-mem elt-sep enum-mem-seq ($$ (tl-insert $3 $1))))
    (enum-mem
     (ident ($$ `(enum $1)))
     (ident "=" expr ($$ `(enum $1 $3))))

    (struct-defn
     (struct-defn-1 ($$ (reverse $1))))
    (struct-defn-0
     ("struct" ident "{" struct-mem-seq "}"
      ($$ (list (tl->list $4) $2 'struct-defn))))
    (struct-defn-1
     (struct-defn-0)
     (struct-defn-0 "default" expr ($$ (cons `(default ,$3) $1))))
    (struct-mem-seq
     ($empty ($$ (make-tl 'mem-seq)))
     (struct-mem mem-sep struct-mem-seq ($$ (tl-insert $3 $1))))
    (struct-mem (struct-mem-3 ($$ (reverse $1))))
    (struct-mem-0 (ident ":" ($$ (list $1 'struct-elt))))
    (struct-mem-1 (struct-mem-0)
                  (struct-mem-0 "[" expr "]" ($$ (cons `(index ,$3) $1))))
    (struct-mem-2 (struct-mem-1 type-name ($$ (cons $2 $1))))
    (struct-mem-3 (struct-mem-2)
                  (struct-mem-2 "format" string ($$ (cons `(format ,$3) $1))))

    (port-defn (port-defn-2 ($$ reverse $1)))
    (port-defn-0 ("port" ident ($$ (list ident 'port-defn))))
    (port-defn-1 (port-defn-0)
                 (port-defn-0 "(" param-list ")" ($$ (cons $3 $1))))
    (port-defn-2 (port-defn-1)
                 (port-defn-1 "->" type-name ($$ (cons $3 $1))))


    ;; === component spec ===============

    (component-defn
     (comp-kind ident "{" comp-mem-seq "}"
                ($$ `(comp-defn (@ (kind ,$1)) ,$2 ,(reverse $4)))))
    (comp-kind ("active") ("passive") ("queued"))
    (comp-mem-seq
     ($empty ($$ (make-tl 'mem-seq)))
     (comp-mem mem-sep comp-mem-seq ($$ (tl-insert $3 $1))))
    (comp-mem
     (include-spec)
     (enum-defn)
     (struct-defn)
     (int-port-spec)
     (port-inst)
     (port-match-spec)
     (command-spec)
     (event-spec)
     (param-spec)
     (record-spec)
     (prod-cont-spec)
     (tlm-chan-spec)
     ;;(stmach-defn)
     ;;(stmach-inst)
     )

    (port-inst
     (gen-port-inst-4)
     (spc-port-inst-3))
    
    (gen-port-inst-0
     (input-port-kind "input" "port" ident ":")
     ("output" "port" ident ":"))
    (gen-port-inst-1 (gen-port-inst-0) (gen-port-inst-0 expr))
    (gen-port-inst-2 (gen-port-inst-1 qual-ident) (gen-port-inst-1 "serial"))
    (gen-port-inst-3 (gen-port-inst-2) (gen-port-inst-2 "priority" expr))
    (gen-port-inst-4 (gen-port-inst-3) (gen-port-inst-3 queue-full-beh))

    (spc-port-inst-0 (spc-port-kind "port" ident ":"))
    (spc-port-inst-1 (spc-port-inst-0) (input-port-kind spc-port-inst-0))
    (spc-port-inst-2 (spc-port-inst-1) (spc-port-inst-1 "priority" expr))
    (spc-port-inst-3 (spc-port-inst-2) (spc-port-inst-2 queue-full-beh))
    
    (int-port-spec (int-port-defn-3))
    (int-port-defn-0 ("internal" "port" ident))
    (int-port-defn-1 (int-port-defn-0) (int-port-defn-0 "(" param-list ")"))
    (int-port-defn-2 (int-port-defn-1) (int-port-defn-1 "priority" expr))
    (int-port-defn-3 (int-port-defn-2) (int-port-defn-2 queue-full-beh))

    (command-spec (cmd-spec-4))
    (cmd-spec-0 (input-port-kind "command" ident))
    (cmd-spec-1 (cmd-spec-0) (cmd-spec-0 "(" param-list ")"))
    (cmd-spec-2 (cmd-spec-1) (cmd-spec-1 "opcode" expr))
    (cmd-spec-3 (cmd-spec-2) (cmd-spec-2 "priority" expr))
    (cmd-spec-4 (cmd-spec-3) (cmd-spec-3 queue-full-beh))
    
    (spc-port-kind
     ("command" "recv")
     ("command" "reg")
     ("command" "resp")
     ("event")
     ("param" "get")
     ("param" "set")
     ("product" "get")
     ("product" "recv")
     ("product" "request")
     ("product" "send")
     ("telemetry")
     ("text" "event")
     ("time" "get")
     )

    (input-port-kind
     ("async" ($$ 'async))
     ("guarded" ($$ 'guarded))
     ("sync" ($$ 'async)))

    (event-spec (event-spec-5))
    (event-spec-0 ("event" ident))
    (event-spec-1 (event-spec-0) (event-spec-0 "(" param-list ")"))
    (event-spec-2 (event-spec-1 "severity" expr))
    (event-spec-3 (event-spec-2) (event-spec-2 "id" expr))
    (event-spec-4 (event-spec-3 "format" string))
    (event-spec-5 (event-spec-4) (event-spec-4 "throttle" expr))

    (param-spec (param-spec-4))
    (param-spec-0 ("param" ident ":" type-name))
    (param-spec-1 (param-spec-0) (param-spec-0 "default" expr))
    (param-spec-2 (param-spec-1) (param-spec-1 "id" expr))
    (param-spec-3 (param-spec-2) (param-spec-2 "set" "opcode" expr))
    (param-spec-4 (param-spec-3) (param-spec-3 "save" "opcode" expr))

    (tlm-chan-spec (tlm-chan-5))
    (tlm-chan-0 ("telemetry" ident ":" type-name))
    (tlm-chan-1 (tlm-chan-0) (tlm-chan-0 "id" expr))
    (tlm-chan-2 (tlm-chan-1) (tlm-chan-1 "update" tlm-update))
    (tlm-chan-3 (tlm-chan-2) (tlm-chan-2 "format" string))
    (tlm-chan-4 (tlm-chan-3) (tlm-chan-3 "low" "{" tlm-lim-seq "}"))
    (tlm-chan-5 (tlm-chan-4) (tlm-chan-4 "high" "{" tlm-lim-seq "}"))
    (tlm-update ("always" ($$ 'always)) ("on" "change" ($$ 'on-change)))
    (tlm-lim-seq (tlm-lim) (tlm-lim-seq elt-sep tlm-lim))
    (tlm-lim ("red" expr) ("orange" expr) ("yellow" expr))

    (record-spec (record-spec-2))
    (record-spec-0 ("product" "record" ident ":" type-name))
    (record-spec-1 (record-spec-0) (record-spec-0 "array"))
    (record-spec-2 (record-spec-1) (record-spec-1 "id" expr))

    (prod-cont-spec (cont-spec-2))
    (cont-spec-0 ("product" "container" ident))
    (cont-spec-1 (cont-spec-0) (cont-spec-0 "id" expr))
    (cont-spec-2 (cont-spec-1) (cont-spec-1 "default" "priority" expr))


    ;; === instance spec ================
    
    (comp-inst-defn (comp-inst-7))
    (comp-inst-0 ("instance" ident ":" qual-ident "base" "id" expr
                  ($$ `(comp-inst-defn ,$2 ,$4 ,$7))))
    (comp-inst-1 (comp-inst-0)
                 (comp-inst-0
                  "type" string ($$ (append $1 (list `(type ,$3))))))
    (comp-inst-2 (comp-inst-1)
                 (comp-inst-1
                  "at" string ($$ (append $1 (list `(type ,$3))))))
    (comp-inst-3 (comp-inst-2)
                 (comp-inst-2
                  "queue" "size" expr ($$ (append $1 (list `(qsiz ,$4))))))
    (comp-inst-4 (comp-inst-3)
                 (comp-inst-3
                  "stack" "size" expr ($$ (append $1 (list `(stksiz ,$4))))))
    (comp-inst-5 (comp-inst-4)
                 (comp-inst-4
                  "priority" expr ($$ (append $1 (list `(prio ,$3))))))
    (comp-inst-6 (comp-inst-5)
                 (comp-inst-5
                  "cpu" expr ($$ (append $1 (list `(cpu ,$3))))))
    (comp-inst-7 (comp-inst-6)
                 (comp-inst-6
                  "{" string "}" ($$ (append $1 (list `(code ,$3))))))


    ;; === topology spec ================

    (topology-defn
     ("topology" ident "{" topo-mem-seq "}"))
    (topo-mem-seq
     ($empty)
     (topo-mem mem-sep topo-mem-seq))
    (topo-mem
     (comp-inst-spec)
     (conn-graph-spec)
     (tlm-pktset-spec)
     ("import" qual-ident)
     (include-spec))
    
    (comp-inst-spec
     ("instance" ident)
     ("private" "instance" ident))

    (conn-graph-spec
     ("connections" ident "{" conn-seq "}")
     (pattern-kind "connections" "instance" qual-ident)
     (pattern-kind "connections" "instance" qual-ident "{" qual-ident-seq "}" ))
    (pattern-kind
     ("command" ($$ 'command))
     ("event" ($$ 'event))
     ("health" ($$ 'health))
     ("param" ($$ 'command))
     ("telemetry" ($$ 'command))
     ("text" "event" ($$ 'text-event))
     ("time" ($$ 'time)))

    (conn-seq
     ($empty)
     (connection elt-sep conn-seq))
    (connection (connection-4))
    (connection-0 (qual-ident))
    (connection-1 (connection-0) ("unmatched" connection-0))
    (connection-2 (connection-1) (connection-1 "[" expr "]"))
    (connection-3 (connection-2 "->" qual-ident))
    (connection-4 (connection-3) (connection-3 "[" expr "]"))

    (tlm-pktset-spec (tlm-pktset-spec-1))
    (tlm-pktset-spec-0     
     ("telemetry" "packets" ident "{" tlm-pktgrp-mem-seq "}"))
    (tlm-pktset-spec-1
     (tlm-pktset-spec-0)
     (tlm-pktset-spec-0 "omit" "{" tlm-chan-id-seq "}"))

    (tlm-pktgrp-mem-seq
     ($empty)
     (tlm-pktgrp-mem elt-sep tlm-pktgrp-mem-seq))
    (tlm-pktgrp-mem
     (include-spec)
     (tlm-pkt-spec))
    
    (tlm-pkt-spec (tlm-pkt-spec-2))
    (tlm-pkt-spec-0 ("packet" ident))
    (tlm-pkt-spec-1 (tlm-pkt-spec-0) (tlm-pkt-spec-0 "id" expr))
    (tlm-pkt-spec-2 (tlm-pkt-spec-1 "group" expr "{" tlm-pkt-mem-seq "}" ))

    (tlm-pkt-mem-seq
     ($empty)
     (tlm-pkt-mem elt-sep tlm-pkt-mem-seq))
    (tlm-pkt-mem
     (include-spec)
     (qual-ident))

    (tlm-chan-id-seq
     (qual-ident)
     (tlm-chan-id-seq elt-sep qual-ident))


    ;; ==================================

    (param-list
     (formal-param)
     (param-list elt-sep formal-param))
    (formal-param
     (ident ":" type-name)
     ("ref" ident ":" type-name))

    (queue-full-beh
     ("assert" ($$ 'assert))
     ("block" ($$ 'block))
     ("drop" ($$ 'drop))
     ("hook" ($$ 'hook)))

    ;;(specLoc
    (loc-spec
     ("locate" "instance" qual-ident "at" string)
     ("locate" "component" qual-ident "at" string)
     ("locate" "cnstant" qual-ident "at" string)
     ("locate" "port" qual-ident "at" string)
     ("locate" "state" "machine" qual-ident "at" string)
     ("locate" "topology" qual-ident "at" string)
     ("locate" "type" qual-ident "at" string)
     )

    ;;(specPortMatching)
    (port-match-spec
     ("match" ident "with" ident)
     )

    ;; === expr's and prim's ===========

    (expr (add-expr))
    (add-expr
     (mul-expr)
     (add-expr "+" mul-expr)
     (add-expr "-" mul-expr))
    (mul-expr
     (unary-expr)
     (mul-expr "*" unary-expr)
     (mul-expr "/" unary-expr))
    (unary-expr
     (prim-expr)
     ("-" unary-expr))
    (prim-expr
     (qual-ident)
     (number)
     (string)
     ("[" expr-seq "]")
     ("{" struct-elt-seq "}")
     ("(" expr ")"))
    (expr-seq
     (expr)
     (expr elt-sep expr))
    (struct-elt-seq
     ($empty)
     (ident "=" expr elt-sep struct-elt-seq))

    (number ($float ($$ `(float ,$1)))
            ($fixed ($$ `(fixed ,$1))))
    (ident ($ident ($$ `(ident ,$1))))
    (string ($string ($$ `(string ,$1))))

    (qual-ident (qual-ident-1 ($$ (tl->list $1))))
    (qual-ident-1
     (ident ($$ (make-tl 'qual-ident (sx-ref $1 1))))
     (qual-ident-1 "." ident ($$ (tl-append $1 (sx-ref $3 1)))))
    (qual-ident-seq
     (qual-ident)
     (qual-ident-seq elt-sep qual-ident))

    (index
     ("[" expr "]" ($$ `(index $2))))

    (type-name
     ("I8" ($$ `(type-name $1))) ("U8" ($$ `(type-name $1)))
     ("I16" ($$ `(type-name $1))) ("U16" ($$ `(type-name $1)))
     ("I32" ($$ `(type-name $1))) ("U32" ($$ `(type-name $1)))
     ("I64" ($$ `(type-name $1))) ("U64" ($$ `(type-name $1)))
     ("F32" ($$ `(type-name $1))) ("F64" ($$ `(type-name $1)))
     ("bool" ($$ `(type-name $1))) ("string" ($$ `(type-name $1)))
     ("string" "size" expr ($$ `(type-name (@ (size ,$3)) $1))))


    ;; === state machines ==============
    #|
    (stmach-inst (stmach-inst-2))
    (stmach-inst-0 ("state" "machine" "instance" ident ":" qual-ident))
    (stmach-inst-1 (stmach-inst-0) (stmach-inst-0 "priority" expr))
    (stmach-inst-2 (stmach-inst-1) (stmach-inst-1 queue-full-beh))
    
    (stmach-defn
      ("state" "machine" ident)
      ("state" "machine" ident "{" stmach-membe-seq "}"))

    (stmach-member-seq
      (stmach-member)
      (stmach-member-seq mem-sep stmach-member))
    (stmach-member
     )

    (specInit)

    (state-defn
     ("state" ident)
     ("state" ident "{" state-def-member-seq "}"))

    (specStateEntry
     ("entry" do-expr)
     )
    (specStateExit
     ("exit" do-expr)
     )

    (specInitialTransition
     ("initial" transitionExpr)
    )

    (specStateTransition
     ("on" ident transition-or-do)
     ("on" ident "if" ident transition-or-do)
     )

    (signal-defn
     ("signal" ident)
     ("signal" ident ":" type-name))

    (defGuard
      (ident)
      (ident ":" typeName)
      )

    (action-defn
      (ident ":" typeName)
      (ident))

    (action-seq
     (ident)
     (action-seq elt-sep ident))

    (choice-defn
      ("choice" ident "{" "if" ident transExpr "else" transExpr "}")) 

    (transitionOrDo    )
    
    (transitionExpr
     (do-expr "enter" qual-ident)
     ("enter" qual-ident)
     )

    (do-expr
      ("do" "{" action-seq "}"))
    
    |#
    )))

(define fpp-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine fpp-spec))
   #:keep 0 #:keepers '("\n" $code-anno $lone-anno)))

;; === automaton file generators =========

(define (gen-fpp-files . rest)
  (define (lang-dir path)
    (if (pair? rest) (string-append (car rest) "/" path) path))
  (define (xtra-dir path) (lang-dir (string-append "mach.d/" path)))
  (write-lalr-actions fpp-mach (xtra-dir "fpp-act.scm.new") #:prefix "fpp-")
  (write-lalr-tables fpp-mach (xtra-dir "fpp-tab.scm.new") #:prefix "fpp-")
  (sleep 1) ;; may be needed un
  (or (move-if-changed (xtra-dir "fpp-act.scm.new") (xtra-dir "fpp-act.scm"))
      (move-if-changed (xtra-dir "fpp-tab.scm.new") (xtra-dir "fpp-tab.scm"))))

;; --- last line ---
