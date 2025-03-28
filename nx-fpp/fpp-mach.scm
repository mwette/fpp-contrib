;;; fpp-mach.scm

;; Copyright (C) 2025 Matthew Wette
;; SPDX-License-Identifier: Apache-2.0

;; refs:
;; * https://github.com/nasa/fpp/blob/
;;           main/compiler/lib/src/main/scala/syntax/Parser.scala
;; * https://nasa.github.io/fpp/fpp-spec.html
;; * https://www.nongnu.org/nyacc/nyacc-ug.html

;;; Notes:

;; annotations can occur at
;; 1. definition (done)
;; 2. specification (done
;; 3. member of struct defn (done)
;; 4. formal parameter (done)
;; 5. state machine member
;; 6. state definition member

;;; Code:

(define-module (fpp-mach)
  #:export (fpp-spec fpp-mach gen-fpp-files)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse))

;; fails as no newline at end of file

(define fpp-spec
  (lalr-spec
   (notice (string-append "Copyright 2025 Matthew Wette\n"
                          "SPDX-License-Identifier: Apache-2.0"))
   ;;(prec< '$ident 'dotted-id) ; couldn't get this to work
   (expect 1)
   (start translation-unit)
   (reserve '$lone-anno '$code-anno '$lone-comm '$code-comm)
   (grammar

    (elt-sep (",") ("\n") (elt-sep "\n"))
    (mem-sep (";") ("\n") (mem-sep "\n"))

    (include-spec
     ("include" $string
      ($$ (let* ((path (dirname (port-filename (current-input-port))))
                 (path (string-append path "/" $2)))
            (push-input (open-input-file path))))))

    (translation-unit
     (module-mem-seq ($$ `(fpp-trans-unit ,@(sx-tail (seq->elt $1))))))

    (module-mem-seq
     ($empty ($$ (make-seq)))
     (mod-mem ($$ (seq-insert (make-seq) $1)))
     (include-spec ($$ (seq-insert (make-seq) $1)))
     (mod-mem mem-sep module-mem-seq ($$ (seq-insert $3 $1)))
     (include-spec mem-sep module-mem-seq ($$ $3)))
    (mod-mem
     (lone-anno)
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
     (stmach-defn))

    (module-defn
     ("module" ident "{" module-mem-seq "}"
      ($$ `(module-defn ,$2 ,(seq->elt $4)))))

    ;; === const, abs-type, array, enum, struct, port defn's
    
    (const-defn
     ("constant" ident "=" expr ($$ `(const-defn ,$2 ,$4)))
     ("constant" ident "=" expr code-anno
      ($$ `(const-defn (@ (anno ,$4)) ,$2 ,$4))))
    (abs-type-defn
     ("type" ident ($$ `(type-defn ,$2)))
     ("type" ident code-anno ($$ `(type-defn (@ (anno ,$3)) ,$2))))
    (array-defn
     ("array" ident "=" index ident ($$ `(array-defn ,$2 ,$4 ,$5)))
     ("array" ident "=" index ident code-anno
      ($$ `(array-defn (@ (anno ,$6)) ,$2 ,$4 ,$5))))
    
    (enum-defn
     (enum-defn-4 ($$ (if (string? (car $1)) (annoverse $1) (reverse $1)))))
    (enum-defn-0 ("enum" ident ($$ (list $2 'enum-defn))))
    (enum-defn-1 (enum-defn-0)
                 (enum-defn-0 ":" type-name ($$ (cons $3 $1))))
    (enum-defn-2 (enum-defn-1 "{" enum-mem-seq "}" ($$ (cons (seq->elt $3) $1))))
    (enum-defn-3 (enum-defn-2)
                 (enum-defn-2 "default" expr ($$ (cons `(default ,$3) $1))))
    (enum-defn-4 (enum-defn-3)
                 (enum-defn-3 code-anno ($$ (cons $2 $1))))
    (enum-mem-seq
     ($empty ($$ (make-seq)))
     (enum-mem ($$ (seq-insert (make-seq) $1)))
     (enum-mem elt-sep enum-mem-seq ($$ (seq-insert $3 $1))))
    (enum-mem
     (ident ($$ `(enum ,$1)))
     (ident code-anno ($$ `(enum (@ (anno ,$2)),$1)))
     (ident "=" expr ($$ `(enum ,$1 ,$3)))
     (ident "=" expr code-anno ($$ `(enum (@ (anno ,$4)) ,$1 ,$3))))

    (struct-defn
     (struct-defn-2 ($$ (if (string? (car $1)) (annoverse $1) (reverse $1)))))
    (struct-defn-0
     ("struct" ident "{" struct-mem-seq "}"
      ($$ (list (seq->elt $4) $2 'struct-defn))))
    (struct-defn-1 (struct-defn-0)
                   (struct-defn-0 "default" expr ($$ (cons `(default ,$3) $1))))
    (struct-defn-2 (struct-defn-1)
                   (struct-defn-1 code-anno ($$ (cons $2 $1))))
    (struct-mem-seq
     ($empty ($$ (make-seq)))
     (struct-mem ($$ (seq-insert (make-seq) $1)))
     (struct-mem mem-sep struct-mem-seq ($$ (seq-insert $3 $1))))
    (struct-mem
     (struct-mem-3 ($$ (reverse $1)))
     (struct-mem-3 code-anno
                   ($$ (let (($1 (reverse $1)))
                         (cons* (car $1) `(@ (anno ,$2)) (cdr $1))))))
    (struct-mem-0 (ident ":" ($$ (list $1 'struct-elt))))
    (struct-mem-1 (struct-mem-0)
                  (struct-mem-0 "[" expr "]" ($$ (cons `(index ,$3) $1))))
    (struct-mem-2 (struct-mem-1 type-name ($$ (cons $2 $1))))
    (struct-mem-3 (struct-mem-2)
                  (struct-mem-2 "format" string ($$ (cons `(format ,$3) $1))))

    (port-defn
     (port-defn-3 ($$ (if (string? (car $1)) (annoverse $1) (reverse $1)))))
    (port-defn-0 ("port" ident ($$ (list $2 'port-defn))))
    (port-defn-1 (port-defn-0)
                 (port-defn-0 "(" param-list ")" ($$ (cons $3 $1))))
    (port-defn-2 (port-defn-1)
                 (port-defn-1 "->" type-name ($$ (cons $3 $1))))
    (port-defn-3 (port-defn-2)
                 (port-defn-2 code-anno ($$ (cons $2 $1))))


    ;; === component spec ===============

    (component-defn
     (comp-kind "component" ident "{" comp-mem-seq "}"
                ($$ `(component-defn ,$3 (kind ,$1) ,(seq->elt $5)))))
    (comp-kind ("active") ("passive") ("queued"))
    (comp-mem-seq
     ($empty ($$ (make-seq)))
     (comp-mem mem-sep comp-mem-seq ($$ (seq-insert $3 $1)))
     (include-spec mem-sep comp-mem-seq ($$ $3)))
    (comp-mem
     (lone-anno)
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
     (stmach-defn)
     (stmach-inst))

    (port-inst
     (gen-port-inst-4 ($$ (reverse $1)))
     (spc-port-inst-3 ($$ (reverse $1))))
    
    (gen-port-inst-0
     (input-port-kind "input" "port" ident ":"
                      ($$ (list `(kind ,$1) $4 'input-port)))
     ("output" "port" ident ":" ($$ (list $3 'output-port))))
    (gen-port-inst-1 (gen-port-inst-0)
                     (gen-port-inst-0 "[" expr "]" ($$ (cons $3 $1))))
    (gen-port-inst-2 (gen-port-inst-1 qual-ident ($$ (cons $2 $1)))
                     (gen-port-inst-1 "serial" ($$ (cons $2 $1))))
    (gen-port-inst-3 (gen-port-inst-2)
                     (gen-port-inst-2 "priority" expr
                                      ($$ (cons `(prio ,$3) $1))))
    (gen-port-inst-4 (gen-port-inst-3)
                     (gen-port-inst-3 queue-full-beh ($$ (cons $2 $1))))

    (spc-port-inst-0
     (spc-iport-kind "port" ident ($$ (list `(spc-kind ,$1) $3 'input-port)))
     (spc-oport-kind "port" ident ($$ (list `(spc-kind ,$1) $3 'output-port))))
    (spc-port-inst-1 (spc-port-inst-0)
                     (input-port-kind spc-port-inst-0
                                      ($$ (cons `(kind ,$1) $2))))
    (spc-port-inst-2 (spc-port-inst-1)
                     (spc-port-inst-1 "priority" expr
                                      ($$ (cons `(priority $3) $1))))
    (spc-port-inst-3 (spc-port-inst-2)
                     (spc-port-inst-2 queue-full-beh ($$ (cons $2 $1))))
    
    (int-port-spec (int-port-defn-3 ($$ (reverse $1))))
    (int-port-defn-0 ("internal" "port" ident ($$ (list $3 'int-port))))
    (int-port-defn-1 (int-port-defn-0)
                     (int-port-defn-0 "(" param-list ")" ($$ (cons $3 $1))))
    (int-port-defn-2 (int-port-defn-1)
                     (int-port-defn-1 "priority" expr ($$ (cons `(prio $3) $1))))
    (int-port-defn-3 (int-port-defn-2)
                     (int-port-defn-2 queue-full-beh ($$ (cons $2 $1))))

    (command-spec
     (cmd-spec-4 ($$ (reverse $1))))
    (cmd-spec-0 (input-port-kind "command" ident
                                 ($$ (list `(kind ,$1) $3 'command))))
    (cmd-spec-1 (cmd-spec-0)
                (cmd-spec-0 "(" param-list ")" ($$ (cons $3 $1))))
    (cmd-spec-2 (cmd-spec-1)
                (cmd-spec-1 "opcode" expr ($$ (cons `(opcode ,$3) $1))))
    (cmd-spec-3 (cmd-spec-2)
                (cmd-spec-2 "priority" expr ($$ (cons `(prio $3) $1))))
    (cmd-spec-4 (cmd-spec-3)
                (cmd-spec-3 queue-full-beh ($$ (cons $2 $1))))
    
    (spc-iport-kind
     ("command" "reg" ($$ "command-reg"))
     ("command" "resp" ($$ "command-resp"))
     ("event")
     ("param" "get" ($$ "param-get"))
     ("param" "set" ($$ "param-set"))
     ("product" "get" ($$ "product-get"))
     ("product" "request" ($$ "product-request"))
     ("product" "send" ($$ "product-send"))
     ("telemetry")
     ("text" "event" ($$ "text-event"))
     ("time" "get" ($$ "time-get")))
    (spc-oport-kind
     ("command" "recv" ($$ "command-receive"))
     ("product" "recv" ($$ "product-recv")))

    (input-port-kind ("async") ("guarded") ("sync"))

    (event-spec (event-spec-5 ($$ (reverse $1))))
    (event-spec-0 ("event" ident ($$ (list $2 'event-spec))))
    (event-spec-1 (event-spec-0)
                  (event-spec-0 "(" param-list ")" ($$ (cons $3 $1))))
    (event-spec-2 (event-spec-1 "severity" severity
                                ($$ (cons `(severity ,$3) $1))))
    (event-spec-3 (event-spec-2)
                  (event-spec-2 "id" expr ($$ (cons `(id ,$3) $1))))
    (event-spec-4 (event-spec-3 "format" string ($$ (cons `(format ,$3) $1))))
    (event-spec-5 (event-spec-4)
                  (event-spec-4 "throttle" expr ($$ (cons `(throttle ,$3) $1))))
    (severity
     ("activity" "high" ($$ "activity-high"))
     ("activity" "low" ($$ "activity-low"))
     ("command")
     ("diagnostic")
     ("fatal")
     ("warning" "high" ($$ "warning-high"))
     ("warning" "low" ($$ "warning-low")))

    (param-spec (param-spec-4 ($$ (reverse $1))))
    (param-spec-0 ("param" ident ":" type-name ($$ (list $4 $2 'param))))
    (param-spec-1 (param-spec-0)
                  (param-spec-0 "default" expr ($$ (cons `(default ,$3) $1))))
    (param-spec-2 (param-spec-1)
                  (param-spec-1 "id" expr ($$ (cons `(id ,$3) $1))))
    (param-spec-3 (param-spec-2)
                  (param-spec-2 "set" "opcode" expr ($$ (cons `(set ,$4) $1))))
    (param-spec-4 (param-spec-3)
                  (param-spec-3 "save" "opcode" expr ($$ (cons `(save ,$4) $1))))

    (tlm-chan-spec (tlm-chan-5 ($$ (reverse $1))))
    (tlm-chan-0 ("telemetry" ident ":" type-name ($$ (list $4 $2 'telemetry))))
    (tlm-chan-1 (tlm-chan-0)
                (tlm-chan-0 "id" expr ($$ (cons `(id ,$3) $1))))
    (tlm-chan-2 (tlm-chan-1)
                (tlm-chan-1 "update" tlm-update ($$ (cons '(update ,$3) $1))))
    (tlm-chan-3 (tlm-chan-2)
                (tlm-chan-2 "format" string ($$ (cons `(id ,$3) $1))))
    (tlm-chan-4 (tlm-chan-3)
                (tlm-chan-3 "low" "{" tlm-lim-seq "}"
                            ($$ (cons `(id ,(seq->elt $4)) $1))))
    (tlm-chan-5 (tlm-chan-4)
                (tlm-chan-4 "high" "{" tlm-lim-seq "}"
                            ($$ (cons `(id ,(seq->elt $4)) $1))))
    (tlm-update
     ("always" ($$ "always"))
     ("on" "change" ($$ "on-change")))
    (tlm-lim-seq
     ($empty ($$ (make-seq)))
     (tlm-lim ($$ (seq-insert (make-seq) $1)))
     (tlm-lim elt-sep tlm-lim-seq ($$ (seq-insert $3 $1))))
    (tlm-lim
     ("red" expr ($$ `(tlm-lim ,$1 ,$2)))
     ("orange" expr ($$ `(tlm-lim ,$1 ,$2)))
     ("yellow" expr ($$ `(tlm-lim ,$1 ,$2))))

    (record-spec (record-spec-2 ($$ (reverse $1))))
    (record-spec-0 ("product" "record" ident ":" type-name
                    ($$ (list $5 $3 'prod-recd))))
    (record-spec-1 (record-spec-0)
                   (record-spec-0 "array" ($$ (cons '(array) $1))))
    (record-spec-2 (record-spec-1)
                   (record-spec-1 "id" expr ($$ (cons '(id ,$3) $1))))

    (prod-cont-spec (cont-spec-2))
    (cont-spec-0 ("product" "container" ident
                  ($$ (list $3 'prod-cont))))
    (cont-spec-1 (cont-spec-0)
                 (cont-spec-0 "id" expr ($$ (cons '(id ,$3) $1))))
    (cont-spec-2 (cont-spec-1)
                 (cont-spec-1 "default" "priority" expr
                              ($$ (cons `(def-prio ,$4) $1))))


    ;; === instance spec ================
    
    (comp-inst-defn (comp-inst-7 ($$ (reverse $1))))
    (comp-inst-0 ("instance" ident ":" qual-ident "base" "id" expr
                  ($$ (list `(id ,$7) $4 $2 'instance))))
    (comp-inst-1 (comp-inst-0)
                 (comp-inst-0 "type" string ($$ (cons `(type ,$3) $1))))
    (comp-inst-2 (comp-inst-1)
                 (comp-inst-1 "at" string ($$ (cons `(type ,$3) $1))))
    (comp-inst-3 (comp-inst-2)
                 (comp-inst-2 "queue" "size" expr ($$ (cons `(qsiz ,$4) $1))))
    (comp-inst-4 (comp-inst-3)
                 (comp-inst-3 "stack" "size" expr ($$ (cons `(stksiz ,$4) $1))))
    (comp-inst-5 (comp-inst-4)
                 (comp-inst-4 "priority" expr ($$ (cons `(prio ,$3) $1))))
    (comp-inst-6 (comp-inst-5)
                 (comp-inst-5 "cpu" expr ($$ (cons `(cpu ,$3) $1))))
    (comp-inst-7 (comp-inst-6)
                 (comp-inst-6 "{" string "}" ($$ (cons `(code ,$3) $1))))


    ;; === topology spec ================

    (topology-defn
     ("topology" ident "{" topo-mem-seq "}"
      ($$ `(topology-defn ,$2 ,(seq->elt $4)))))
    (topo-mem-seq
     ($empty ($$ (make-seq)))
     (topo-mem ($$ (seq-insert (make-seq) $1)))
     (include-spec ($$ (seq-insert (make-seq) $1)))
     (topo-mem mem-sep topo-mem-seq ($$ (seq-insert $3 $1)))
     (include-spec mem-sep topo-mem-seq ($$ $3)))
    (topo-mem
     (lone-anno)
     (comp-inst-spec)
     (conn-graph-spec)
     (tlm-pktset-spec)
     ("import" qual-ident ($$ `(import ,$2))))
    
    (comp-inst-spec
     ("instance" ident ($$ `(comp-inst ,$2)))
     ("private" "instance" ident ($$ `(comp-priv-inst ,$3))))

    (conn-graph-spec
     ("connections" ident "{" conn-seq "}"
      ($$ `(connections ,$2 ,(seq->elt $4))))
     (pattern-kind "connections" "instance" qual-ident
                   ($$ `(connections-inst ,$4 (kind ,$1))))
     (pattern-kind "connections" "instance" qual-ident "{" qual-ident-seq "}"
                   ($$ `(connections-inst ,$4 (kind ,$1) ,(seq->elt $6)))))
    (pattern-kind
     ("command") ("event") ("health") ("param") ("telemetry") ("time")
     ("text" "event" ($$ "text-event")))

    (conn-seq
     ($empty ($$ (make-seq)))
     (connection ($$ (seq-insert (make-seq) $1)))
     (connection elt-sep conn-seq ($$ (seq-insert $3 $1))))
    (connection
     (conn-from "->" conn-to ($$ `(conn ,$1 ,$3)))
     ("unmatched" conn-from "->" conn-to ($$ `(unmatched-conn ,$1 ,$3))))
    (conn-from
     (qual-ident ($$ `(from ,$1)))
     (qual-ident "[" expr "]" ($$ `(from ,$1 ,$3))))
    (conn-to
     (qual-ident ($$ `(to ,$1)))
     (qual-ident "[" expr "]" ($$ `(to ,$1 ,$3))))

    (tlm-pktset-spec
     ("telemetry" "packets" ident "{" tlm-pktgrp-mem-seq "}"
      ($$ `(tlm-packets ,$3 ,(seq->elt $4))))
     ("telemetry" "packets" ident "{" tlm-pktgrp-mem-seq "}"
      "omit" "{" tlm-chan-id-seq "}"
      ($$ `(tlm-packets ,$3 ,(seq->elt $5) (omit ,@(sx-tail (seq->elt $9)))))))

    (tlm-pktgrp-mem-seq
     ($empty ($$ (make-seq)))
     (tlm-pkt-spec ($$ (seq-insert (make-seq) $1)))
     (include-spec ($$ (seq-insert (make-seq) $1)))
     (tlm-pkt-spec elt-sep tlm-pktgrp-mem-seq ($$ (seq-insert $3 $1)))
     (include-spec elt-sep tlm-pktgrp-mem-seq ($$ $3)))
    
    (tlm-pkt-spec
     ("packet" ident "group" expr "{" tlm-pkt-mem-seq "}"
      ($$ `(packet ,$2 (group ,$4) (seq->elt $6))))
     ("packet" ident "group" expr "id" expr "{" tlm-pkt-mem-seq "}"
      ($$ `(packet ,$2 (group ,$4) (id ,$6) (seq->elt $8)))))

    (tlm-pkt-mem-seq
     ($empty ($$ (make-seq)))
     (qual-ident ($$ (seq-insert (make-seq) $1)))
     (include-spec ($$ (seq-insert (make-seq) $1)))
     (qual-ident elt-sep tlm-pkt-mem-seq ($$ (seq-insert $3 $1)))
     (include-spec elt-sep tlm-pkt-mem-seq ($$ $3)))

    (tlm-chan-id-seq
     (qual-ident-seq))


    ;; === misc =========================

    (param-list (param-list-1 ($$ (seq->elt $1))))
    (param-list-1
     ($empty ($$ (make-seq)))
     (formal-param ($$ (seq-insert (make-seq) $1)))
     (formal-param elt-sep param-list-1 ($$ (seq-insert $3 $1))))
    (formal-param
     (formal-param-1)
     (formal-param-1 code-anno
                     ($$ (cons* (sx-tag $1) `(@ (anno ,$2)) (sx-tail $1)))))
    (formal-param-1
     (ident ":" type-name ($$ `(param ,$1 ,$3)))
     ("ref" ident ":" type-name ($$ `(ref-param ,$2 ,$4))))

    (queue-full-beh
     (queue-full-beh-1 ($$ `(q-full-beh ,$1))))
    (queue-full-beh-1 ("assert") ("block") ("drop") ("hook"))

    (loc-spec
     ("locate" "instance" qual-ident "at" string
      ($$ `(loc-inst ,$3 (at ,$5))))
     ("locate" "component" qual-ident "at" string
      ($$ `(loc-comp ,$3 (at ,$5))))
     ("locate" "constant" qual-ident "at" string
      ($$ `(loc-const ,$3 (at ,$5))))
     ("locate" "port" qual-ident "at" string
      ($$ `(loc-port ,$3 (at ,$5))))
     ("locate" "state" "machine" qual-ident "at" string
      ($$ `(loc-stmach ,$3 (at ,$6))))
     ("locate" "topology" qual-ident "at" string
      ($$ `(loc-topo ,$3 (at ,$5))))
     ("locate" "type" qual-ident "at" string
      ($$ `(loc-type ,$3 (at ,$5)))))

    (port-match-spec
     ("match" ident "with" ident ($$ `(match ,$2 ,$4))))


    ;; === expr's and prim's ===========

    (expr
     (add-expr ($$ `(expr ,$1))))
    (add-expr
     (mul-expr)
     (add-expr "+" mul-expr ($$ `(add ,$1 ,$3)))
     (add-expr "-" mul-expr ($$ `(sub ,$1 ,$3))))
    (mul-expr
     (unary-expr)
     (mul-expr "*" unary-expr ($$ `(mul ,$1 ,$3)))
     (mul-expr "/" unary-expr ($$ `(div ,$1 ,$3))))
    (unary-expr
     (prim-expr)
     ("-" unary-expr ($$ `(neg ,$2))))
    (prim-expr
     (qual-ident)
     (number)
     (string)
     ("[" expr-seq "]" ($$ `(array-val ,@(sx-tail (seq->elt $2)))))
     ("{" struct-elt-seq "}" ($$ `(struct-val ,@(sx-tail (seq->elt $2)))))
     ("(" expr ")" ($$ $2)))
    (expr-seq
     (expr ($$ (make-seq)))
     (expr elt-sep expr-seq ($$ (seq-insert $3 $1))))
    (struct-elt-seq
     ($empty ($$ (make-seq)))
     (struct-elt ($$ (seq-insert (make-seq) $1)))
     (struct-elt elt-sep struct-elt-seq ($$ (seq-insert $3 $1))))
    (struct-elt
     (ident "=" expr ($$ `(bind-struct-elt ,$1 ,$3))))

    (number ($float ($$ `(float ,$1)))
            ($fixed ($$ `(fixed ,$1))))
    (ident ($ident ($$ `(ident ,$1))))
    (string ($string ($$ `(string ,$1))))
    
    (lone-anno
     ($lone-anno ($$ `(lone-anno ,$1))))
    (code-anno
     (code-anno-list ($$ (string-join (reverse $1) "\n"))))
    (code-anno-list
     ($code-anno ($$ (list $1)))
     ;;(code-anno-list "\n" $code-anno ($$ (cons $3 $1))) ;; not working
     )

    (qual-ident (qual-ident-1 ($$ (reverse $1))))
    (qual-ident-1
     (ident ($$ (list (sx-ref $1 1) 'qual-ident)))
     (qual-ident-1 "." ident ($$ (cons (sx-ref $3 1) $1))))
    (qual-ident-seq
     ($empty ($$ (make-seq)))
     (qual-ident ($$ (seq-insert (make-seq) $1)))
     (qual-ident elt-sep qual-ident-seq ($$ (seq-insert $3 $1))))

    (index
     ("[" expr "]" ($$ `(index ,$2))))

    (type-name
     (qual-ident ($$ `(type-name ,$1)))
     ("I8" ($$ `(type-name (fixed ,$1)))) ("U8" ($$ `(type-name (fixed ,$1))))
     ("I16" ($$ `(type-name (fixed ,$1)))) ("U16" ($$ `(type-name (fixed ,$1))))
     ("I32" ($$ `(type-name (fixed ,$1)))) ("U32" ($$ `(type-name (fixed ,$1))))
     ("I64" ($$ `(type-name (fixed ,$1)))) ("U64" ($$ `(type-name (fixed ,$1))))
     ("F32" ($$ `(type-name (float ,$1)))) ("F64" ($$ `(type-name (float ,$1))))
     ("bool" ($$ `(type-name (bool ,$1))))
     ("string" ($$ `(type-name (string ,$1))))
     ("string" "size" expr ($$ `(type-name (string ,$1 (size ,$3))))))


    ;; === state machines ==============

    (stmach-inst
     (stmach-inst-2 ($$ (reverse $1)))
     (stmach-inst-2 code-anno
                    ($$ (let (($1 (reverse $1)))
                          (cons* (car $1) `(@ (anno ,$2)) (cdr $1))))))
    (stmach-inst-0 ("state" "machine" "instance" ident ":" qual-ident
                    ($$ (list $5 $4 'stmach-inst))))
    (stmach-inst-1 (stmach-inst-0)
                   (stmach-inst-0 "priority" expr ($$ (cons `(prio ,$3) $1))))
    (stmach-inst-2 (stmach-inst-1)
                   (stmach-inst-1 queue-full-beh ($$ (cons $2 $1))))
    
    (stmach-defn
     ("state" "machine" ident ($$ `(stmach-defn ,$3)))
     ("state" "machine" ident "{" stmach-mem-seq "}"
      ($$ `(stmach-defn ,$3 ,(seq->elt $5)))))

    (stmach-mem-seq
     ($empty ($$ (make-seq)))
     (stmach-mem ($$ (seq-insert (make-seq) $1)))
     (stmach-mem mem-sep stmach-mem-seq ($$ (seq-insert $3 $1))))

    (stmach-mem
     (lone-anno)
     ("choice" ident "{" "if" ident trans-expr "else" trans-expr "}"
      ($$ `(choice ,$2 ,$5 ,$6 ,$7)))
     ("action" ident ($$ `(action ,$2)))
     ("action" ident ":" type-name ($$ `(action ,$2 ,$4)))
     ("guard" ident ($$ `(guard ,$2)))
     ("guard" ident ":" type-name ($$ `(guard ,$2 ,$4)))
     ("signal" ident ($$ `(signal ,$2)))
     ("signal" ident ":" type-name ($$ `(signal ,$2 ,$4)))
     ("initial" trans-expr ($$ `(initial ,$2)))
     (state-defn))

    (state-defn
     ("state" ident ($$ `(state-defn ,$2)))
     ("state" ident "{" state-defn-mem-seq "}"
      ($$ `(state-defn ,$2 ,(seq->elt $4))))) 

    (state-defn-mem-seq
     ($empty ($$ (make-seq)))
     (state-defn-mem ($$ (seq-insert (make-seq) $1)))
     (state-defn-mem mem-sep state-defn-mem-seq ($$ (seq-insert $3 $1))))
    (state-defn-mem
     (lone-anno)
     ("initial" trans-expr ($$ `(intial ,$2)))
     ("choice" ident "{" "if" ident trans-expr "else" trans-expr "}"
      ($$ `(choice ,$2 ,$5 ,$6 ,$7)))
     (state-defn)
     (state-trans-spec)
     ("entry" do-expr ($$ `(entry ,$2)))
     ("exit" do-expr ($$ `(exit ,$2))))

    (state-trans-spec (st-tran-spec-2))
    (st-tran-spec-0 ("on" ident))
    (st-tran-spec-1 (st-tran-spec-0) (st-tran-spec-0 "if" ident))
    (st-tran-spec-2 (st-tran-spec-1 trans-or-do))

    (trans-expr (trans-expr-1))
    (trans-expr-0 ("enter" qual-ident))
    (trans-expr-1 (trans-expr-0) (do-expr trans-expr-0))

    (do-expr
     ("do" "{" action-seq "}" ($$ `(do-expr ,(seq->elt $3)))))
    (action-seq
     ($empty ($$ (make-seq)))
     (ident ($$ (seq-insert (make-seq) $1)))
     (ident elt-sep action-seq ($$ (seq-insert $3 $1))))

    (trans-or-do (trans-expr) (do-expr)))))

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
