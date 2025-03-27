;; fpp-act.scm

;; Copyright 2025 Matthew Wette
;; SPDX-License-Identifier: Apache-2.0

(define fpp-act-v
  (vector
   ;; $start => translation-unit
   (lambda ($1 . $rest) $1)
   ;; elt-sep => ","
   (lambda ($1 . $rest) $1)
   ;; elt-sep => "\n"
   (lambda ($1 . $rest) $1)
   ;; elt-sep => elt-sep "\n"
   (lambda ($2 $1 . $rest) $1)
   ;; mem-sep => ";"
   (lambda ($1 . $rest) $1)
   ;; mem-sep => "\n"
   (lambda ($1 . $rest) $1)
   ;; mem-sep => mem-sep "\n"
   (lambda ($2 $1 . $rest) $1)
   ;; include-spec => "include" '$string
   (lambda ($2 $1 . $rest)
     (let* ((path (dirname (port-filename (current-input-port))))
            (path (string-append path "/" $2)))
       (push-input (open-input-file path))))
   ;; translation-unit => module-mem-seq
   (lambda ($1 . $rest) `(fpp-trans-unit ,@(sx-tail (seq->elt $1))))
   ;; module-mem-seq => 
   (lambda $rest (make-seq))
   ;; module-mem-seq => mod-mem
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; module-mem-seq => include-spec
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; module-mem-seq => mod-mem mem-sep module-mem-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; module-mem-seq => include-spec mem-sep module-mem-seq
   (lambda ($3 $2 $1 . $rest) $3)
   ;; mod-mem => lone-anno
   (lambda ($1 . $rest) $1)
   ;; mod-mem => component-defn
   (lambda ($1 . $rest) $1)
   ;; mod-mem => comp-inst-defn
   (lambda ($1 . $rest) $1)
   ;; mod-mem => topology-defn
   (lambda ($1 . $rest) $1)
   ;; mod-mem => const-defn
   (lambda ($1 . $rest) $1)
   ;; mod-mem => module-defn
   (lambda ($1 . $rest) $1)
   ;; mod-mem => port-defn
   (lambda ($1 . $rest) $1)
   ;; mod-mem => struct-defn
   (lambda ($1 . $rest) $1)
   ;; mod-mem => loc-spec
   (lambda ($1 . $rest) $1)
   ;; mod-mem => abs-type-defn
   (lambda ($1 . $rest) $1)
   ;; mod-mem => array-defn
   (lambda ($1 . $rest) $1)
   ;; mod-mem => enum-defn
   (lambda ($1 . $rest) $1)
   ;; mod-mem => stmach-defn
   (lambda ($1 . $rest) $1)
   ;; module-defn => "module" ident "{" module-mem-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(module-defn ,$2 ,(seq->elt $4)))
   ;; const-defn => "constant" ident "=" expr
   (lambda ($4 $3 $2 $1 . $rest) `(const-defn ,$2 ,$4))
   ;; const-defn => "constant" ident "=" expr code-anno
   (lambda ($5 $4 $3 $2 $1 . $rest) `(const-defn (@ (anno ,$4)) ,$2 ,$4))
   ;; abs-type-defn => "type" ident
   (lambda ($2 $1 . $rest) `(type-defn ,$2))
   ;; abs-type-defn => "type" ident code-anno
   (lambda ($3 $2 $1 . $rest) `(type-defn (@ (anno ,$3)) ,$2))
   ;; array-defn => "array" ident "=" index ident
   (lambda ($5 $4 $3 $2 $1 . $rest) `(array-defn ,$2 ,$4 ,$5))
   ;; array-defn => "array" ident "=" index ident code-anno
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(array-defn (@ (anno ,$6)) ,$2 ,$4 ,$5))
   ;; enum-defn => enum-defn-4
   (lambda ($1 . $rest) (if (string? (car $1)) (annoverse $1) (reverse $1)))
   ;; enum-defn-0 => "enum" ident
   (lambda ($2 $1 . $rest) (list $2 'enum-defn))
   ;; enum-defn-1 => enum-defn-0
   (lambda ($1 . $rest) $1)
   ;; enum-defn-1 => enum-defn-0 ":" type-name
   (lambda ($3 $2 $1 . $rest) (cons $3 $1))
   ;; enum-defn-2 => enum-defn-1 "{" enum-mem-seq "}"
   (lambda ($4 $3 $2 $1 . $rest) (cons (seq->elt $3) $1))
   ;; enum-defn-3 => enum-defn-2
   (lambda ($1 . $rest) $1)
   ;; enum-defn-3 => enum-defn-2 "default" expr
   (lambda ($3 $2 $1 . $rest) (cons `(default ,$3) $1))
   ;; enum-defn-4 => enum-defn-3
   (lambda ($1 . $rest) $1)
   ;; enum-defn-4 => enum-defn-3 code-anno
   (lambda ($2 $1 . $rest) (cons $2 $1))
   ;; enum-mem-seq => 
   (lambda $rest (make-seq))
   ;; enum-mem-seq => enum-mem
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; enum-mem-seq => enum-mem elt-sep enum-mem-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; enum-mem => ident
   (lambda ($1 . $rest) `(enum ,$1))
   ;; enum-mem => ident code-anno
   (lambda ($2 $1 . $rest) `(enum (@ (anno ,$2)) ,$1))
   ;; enum-mem => ident "=" expr
   (lambda ($3 $2 $1 . $rest) `(enum ,$1 ,$3))
   ;; enum-mem => ident "=" expr code-anno
   (lambda ($4 $3 $2 $1 . $rest) `(enum (@ (anno ,$4)) ,$1 ,$3))
   ;; struct-defn => struct-defn-2
   (lambda ($1 . $rest) (if (string? (car $1)) (annoverse $1) (reverse $1)))
   ;; struct-defn-0 => "struct" ident "{" struct-mem-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) (list (seq->elt $4) $2 'struct-defn))
   ;; struct-defn-1 => struct-defn-0
   (lambda ($1 . $rest) $1)
   ;; struct-defn-1 => struct-defn-0 "default" expr
   (lambda ($3 $2 $1 . $rest) (cons `(default ,$3) $1))
   ;; struct-defn-2 => struct-defn-1
   (lambda ($1 . $rest) $1)
   ;; struct-defn-2 => struct-defn-1 code-anno
   (lambda ($2 $1 . $rest) (cons $2 $1))
   ;; struct-mem-seq => 
   (lambda $rest (make-seq))
   ;; struct-mem-seq => struct-mem
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; struct-mem-seq => struct-mem mem-sep struct-mem-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; struct-mem => struct-mem-3
   (lambda ($1 . $rest) (reverse $1))
   ;; struct-mem => struct-mem-3 code-anno
   (lambda ($2 $1 . $rest)
     (let (($1 (reverse $1))) (cons* (car $1) `(@ (anno ,$2)) (cdr $1))))
   ;; struct-mem-0 => ident ":"
   (lambda ($2 $1 . $rest) (list $1 'struct-elt))
   ;; struct-mem-1 => struct-mem-0
   (lambda ($1 . $rest) $1)
   ;; struct-mem-1 => struct-mem-0 "[" expr "]"
   (lambda ($4 $3 $2 $1 . $rest) (cons `(index ,$3) $1))
   ;; struct-mem-2 => struct-mem-1 type-name
   (lambda ($2 $1 . $rest) (cons $2 $1))
   ;; struct-mem-3 => struct-mem-2
   (lambda ($1 . $rest) $1)
   ;; struct-mem-3 => struct-mem-2 "format" string
   (lambda ($3 $2 $1 . $rest) (cons `(format ,$3) $1))
   ;; port-defn => port-defn-3
   (lambda ($1 . $rest) (if (string? (car $1)) (annoverse $1) (reverse $1)))
   ;; port-defn-0 => "port" ident
   (lambda ($2 $1 . $rest) (list $2 'port-defn))
   ;; port-defn-1 => port-defn-0
   (lambda ($1 . $rest) $1)
   ;; port-defn-1 => port-defn-0 "(" param-list ")"
   (lambda ($4 $3 $2 $1 . $rest) (cons $3 $1))
   ;; port-defn-2 => port-defn-1
   (lambda ($1 . $rest) $1)
   ;; port-defn-2 => port-defn-1 "->" type-name
   (lambda ($3 $2 $1 . $rest) (cons $3 $1))
   ;; port-defn-3 => port-defn-2
   (lambda ($1 . $rest) $1)
   ;; port-defn-3 => port-defn-2 code-anno
   (lambda ($2 $1 . $rest) (cons $2 $1))
   ;; component-defn => comp-kind "component" ident "{" comp-mem-seq "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(component-defn ,$3 (kind ,$1) ,(seq->elt $5)))
   ;; comp-kind => "active"
   (lambda ($1 . $rest) $1)
   ;; comp-kind => "passive"
   (lambda ($1 . $rest) $1)
   ;; comp-kind => "queued"
   (lambda ($1 . $rest) $1)
   ;; comp-mem-seq => 
   (lambda $rest (make-seq))
   ;; comp-mem-seq => comp-mem mem-sep comp-mem-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; comp-mem-seq => include-spec mem-sep comp-mem-seq
   (lambda ($3 $2 $1 . $rest) $3)
   ;; comp-mem => lone-anno
   (lambda ($1 . $rest) $1)
   ;; comp-mem => enum-defn
   (lambda ($1 . $rest) $1)
   ;; comp-mem => struct-defn
   (lambda ($1 . $rest) $1)
   ;; comp-mem => int-port-spec
   (lambda ($1 . $rest) $1)
   ;; comp-mem => port-inst
   (lambda ($1 . $rest) $1)
   ;; comp-mem => port-match-spec
   (lambda ($1 . $rest) $1)
   ;; comp-mem => command-spec
   (lambda ($1 . $rest) $1)
   ;; comp-mem => event-spec
   (lambda ($1 . $rest) $1)
   ;; comp-mem => param-spec
   (lambda ($1 . $rest) $1)
   ;; comp-mem => record-spec
   (lambda ($1 . $rest) $1)
   ;; comp-mem => prod-cont-spec
   (lambda ($1 . $rest) $1)
   ;; comp-mem => tlm-chan-spec
   (lambda ($1 . $rest) $1)
   ;; comp-mem => stmach-defn
   (lambda ($1 . $rest) $1)
   ;; comp-mem => stmach-inst
   (lambda ($1 . $rest) $1)
   ;; port-inst => gen-port-inst-4
   (lambda ($1 . $rest) (reverse $1))
   ;; port-inst => spc-port-inst-3
   (lambda ($1 . $rest) (reverse $1))
   ;; gen-port-inst-0 => input-port-kind "input" "port" ident ":"
   (lambda ($5 $4 $3 $2 $1 . $rest) (list `(kind ,$1) $4 'input-port))
   ;; gen-port-inst-0 => "output" "port" ident ":"
   (lambda ($4 $3 $2 $1 . $rest) (list $3 'output-port))
   ;; gen-port-inst-1 => gen-port-inst-0
   (lambda ($1 . $rest) $1)
   ;; gen-port-inst-1 => gen-port-inst-0 "[" expr "]"
   (lambda ($4 $3 $2 $1 . $rest) (cons $3 $1))
   ;; gen-port-inst-2 => gen-port-inst-1 qual-ident
   (lambda ($2 $1 . $rest) (cons $2 $1))
   ;; gen-port-inst-2 => gen-port-inst-1 "serial"
   (lambda ($2 $1 . $rest) (cons $2 $1))
   ;; gen-port-inst-3 => gen-port-inst-2
   (lambda ($1 . $rest) $1)
   ;; gen-port-inst-3 => gen-port-inst-2 "priority" expr
   (lambda ($3 $2 $1 . $rest) (cons `(prio ,$3) $1))
   ;; gen-port-inst-4 => gen-port-inst-3
   (lambda ($1 . $rest) $1)
   ;; gen-port-inst-4 => gen-port-inst-3 queue-full-beh
   (lambda ($2 $1 . $rest) (cons $2 $1))
   ;; spc-port-inst-0 => spc-iport-kind "port" ident
   (lambda ($3 $2 $1 . $rest) (list `(spc-kind ,$1) $3 'input-port))
   ;; spc-port-inst-0 => spc-oport-kind "port" ident
   (lambda ($3 $2 $1 . $rest) (list `(spc-kind ,$1) $3 'output-port))
   ;; spc-port-inst-1 => spc-port-inst-0
   (lambda ($1 . $rest) $1)
   ;; spc-port-inst-1 => input-port-kind spc-port-inst-0
   (lambda ($2 $1 . $rest) (cons `(kind ,$1) $2))
   ;; spc-port-inst-2 => spc-port-inst-1
   (lambda ($1 . $rest) $1)
   ;; spc-port-inst-2 => spc-port-inst-1 "priority" expr
   (lambda ($3 $2 $1 . $rest) (cons `(priority $3) $1))
   ;; spc-port-inst-3 => spc-port-inst-2
   (lambda ($1 . $rest) $1)
   ;; spc-port-inst-3 => spc-port-inst-2 queue-full-beh
   (lambda ($2 $1 . $rest) (cons $2 $1))
   ;; int-port-spec => int-port-defn-3
   (lambda ($1 . $rest) (reverse $1))
   ;; int-port-defn-0 => "internal" "port" ident
   (lambda ($3 $2 $1 . $rest) (list $3 'int-port))
   ;; int-port-defn-1 => int-port-defn-0
   (lambda ($1 . $rest) $1)
   ;; int-port-defn-1 => int-port-defn-0 "(" param-list ")"
   (lambda ($4 $3 $2 $1 . $rest) (cons $3 $1))
   ;; int-port-defn-2 => int-port-defn-1
   (lambda ($1 . $rest) $1)
   ;; int-port-defn-2 => int-port-defn-1 "priority" expr
   (lambda ($3 $2 $1 . $rest) (cons `(prio $3) $1))
   ;; int-port-defn-3 => int-port-defn-2
   (lambda ($1 . $rest) $1)
   ;; int-port-defn-3 => int-port-defn-2 queue-full-beh
   (lambda ($2 $1 . $rest) (cons $2 $1))
   ;; command-spec => cmd-spec-4
   (lambda ($1 . $rest) (reverse $1))
   ;; cmd-spec-0 => input-port-kind "command" ident
   (lambda ($3 $2 $1 . $rest) (list `(kind ,$1) $3 'command))
   ;; cmd-spec-1 => cmd-spec-0
   (lambda ($1 . $rest) $1)
   ;; cmd-spec-1 => cmd-spec-0 "(" param-list ")"
   (lambda ($4 $3 $2 $1 . $rest) (cons $3 $1))
   ;; cmd-spec-2 => cmd-spec-1
   (lambda ($1 . $rest) $1)
   ;; cmd-spec-2 => cmd-spec-1 "opcode" expr
   (lambda ($3 $2 $1 . $rest) (cons `(opcode ,$3) $1))
   ;; cmd-spec-3 => cmd-spec-2
   (lambda ($1 . $rest) $1)
   ;; cmd-spec-3 => cmd-spec-2 "priority" expr
   (lambda ($3 $2 $1 . $rest) (cons `(prio $3) $1))
   ;; cmd-spec-4 => cmd-spec-3
   (lambda ($1 . $rest) $1)
   ;; cmd-spec-4 => cmd-spec-3 queue-full-beh
   (lambda ($2 $1 . $rest) (cons $2 $1))
   ;; spc-iport-kind => "command" "reg"
   (lambda ($2 $1 . $rest) "command-reg")
   ;; spc-iport-kind => "command" "resp"
   (lambda ($2 $1 . $rest) "command-resp")
   ;; spc-iport-kind => "event"
   (lambda ($1 . $rest) $1)
   ;; spc-iport-kind => "param" "get"
   (lambda ($2 $1 . $rest) "param-get")
   ;; spc-iport-kind => "param" "set"
   (lambda ($2 $1 . $rest) "param-set")
   ;; spc-iport-kind => "product" "get"
   (lambda ($2 $1 . $rest) "product-get")
   ;; spc-iport-kind => "product" "request"
   (lambda ($2 $1 . $rest) "product-request")
   ;; spc-iport-kind => "product" "send"
   (lambda ($2 $1 . $rest) "product-send")
   ;; spc-iport-kind => "telemetry"
   (lambda ($1 . $rest) $1)
   ;; spc-iport-kind => "text" "event"
   (lambda ($2 $1 . $rest) "text-event")
   ;; spc-iport-kind => "time" "get"
   (lambda ($2 $1 . $rest) "time-get")
   ;; spc-oport-kind => "command" "recv"
   (lambda ($2 $1 . $rest) "command-receive")
   ;; spc-oport-kind => "product" "recv"
   (lambda ($2 $1 . $rest) "product-recv")
   ;; input-port-kind => "async"
   (lambda ($1 . $rest) $1)
   ;; input-port-kind => "guarded"
   (lambda ($1 . $rest) $1)
   ;; input-port-kind => "sync"
   (lambda ($1 . $rest) $1)
   ;; event-spec => event-spec-5
   (lambda ($1 . $rest) (reverse $1))
   ;; event-spec-0 => "event" ident
   (lambda ($2 $1 . $rest) (list $2 'event-spec))
   ;; event-spec-1 => event-spec-0
   (lambda ($1 . $rest) $1)
   ;; event-spec-1 => event-spec-0 "(" param-list ")"
   (lambda ($4 $3 $2 $1 . $rest) (cons $3 $1))
   ;; event-spec-2 => event-spec-1 "severity" severity
   (lambda ($3 $2 $1 . $rest) (cons `(severity ,$3) $1))
   ;; event-spec-3 => event-spec-2
   (lambda ($1 . $rest) $1)
   ;; event-spec-3 => event-spec-2 "id" expr
   (lambda ($3 $2 $1 . $rest) (cons `(id ,$3) $1))
   ;; event-spec-4 => event-spec-3 "format" string
   (lambda ($3 $2 $1 . $rest) (cons `(format ,$3) $1))
   ;; event-spec-5 => event-spec-4
   (lambda ($1 . $rest) $1)
   ;; event-spec-5 => event-spec-4 "throttle" expr
   (lambda ($3 $2 $1 . $rest) (cons `(throttle ,$3) $1))
   ;; severity => "activity" "high"
   (lambda ($2 $1 . $rest) "activity-high")
   ;; severity => "activity" "low"
   (lambda ($2 $1 . $rest) "activity-low")
   ;; severity => "command"
   (lambda ($1 . $rest) $1)
   ;; severity => "diagnostic"
   (lambda ($1 . $rest) $1)
   ;; severity => "fatal"
   (lambda ($1 . $rest) $1)
   ;; severity => "warning" "high"
   (lambda ($2 $1 . $rest) "warning-high")
   ;; severity => "warning" "low"
   (lambda ($2 $1 . $rest) "warning-low")
   ;; param-spec => param-spec-4
   (lambda ($1 . $rest) (reverse $1))
   ;; param-spec-0 => "param" ident ":" type-name
   (lambda ($4 $3 $2 $1 . $rest) (list $4 $2 'param))
   ;; param-spec-1 => param-spec-0
   (lambda ($1 . $rest) $1)
   ;; param-spec-1 => param-spec-0 "default" expr
   (lambda ($3 $2 $1 . $rest) (cons `(default ,$3) $1))
   ;; param-spec-2 => param-spec-1
   (lambda ($1 . $rest) $1)
   ;; param-spec-2 => param-spec-1 "id" expr
   (lambda ($3 $2 $1 . $rest) (cons `(id ,$3) $1))
   ;; param-spec-3 => param-spec-2
   (lambda ($1 . $rest) $1)
   ;; param-spec-3 => param-spec-2 "set" "opcode" expr
   (lambda ($4 $3 $2 $1 . $rest) (cons `(set ,$4) $1))
   ;; param-spec-4 => param-spec-3
   (lambda ($1 . $rest) $1)
   ;; param-spec-4 => param-spec-3 "save" "opcode" expr
   (lambda ($4 $3 $2 $1 . $rest) (cons `(save ,$4) $1))
   ;; tlm-chan-spec => tlm-chan-5
   (lambda ($1 . $rest) (reverse $1))
   ;; tlm-chan-0 => "telemetry" ident ":" type-name
   (lambda ($4 $3 $2 $1 . $rest) (list $4 $2 'telemetry))
   ;; tlm-chan-1 => tlm-chan-0
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-1 => tlm-chan-0 "id" expr
   (lambda ($3 $2 $1 . $rest) (cons `(id ,$3) $1))
   ;; tlm-chan-2 => tlm-chan-1
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-2 => tlm-chan-1 "update" tlm-update
   (lambda ($3 $2 $1 . $rest) (cons '(update ,$3) $1))
   ;; tlm-chan-3 => tlm-chan-2
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-3 => tlm-chan-2 "format" string
   (lambda ($3 $2 $1 . $rest) (cons `(id ,$3) $1))
   ;; tlm-chan-4 => tlm-chan-3
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-4 => tlm-chan-3 "low" "{" tlm-lim-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) (cons `(id ,(seq->elt $4)) $1))
   ;; tlm-chan-5 => tlm-chan-4
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-5 => tlm-chan-4 "high" "{" tlm-lim-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) (cons `(id ,(seq->elt $4)) $1))
   ;; tlm-update => "always"
   (lambda ($1 . $rest) "always")
   ;; tlm-update => "on" "change"
   (lambda ($2 $1 . $rest) "on-change")
   ;; tlm-lim-seq => 
   (lambda $rest (make-seq))
   ;; tlm-lim-seq => tlm-lim
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; tlm-lim-seq => tlm-lim elt-sep tlm-lim-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; tlm-lim => "red" expr
   (lambda ($2 $1 . $rest) `(tlm-lim ,$1 ,$2))
   ;; tlm-lim => "orange" expr
   (lambda ($2 $1 . $rest) `(tlm-lim ,$1 ,$2))
   ;; tlm-lim => "yellow" expr
   (lambda ($2 $1 . $rest) `(tlm-lim ,$1 ,$2))
   ;; record-spec => record-spec-2
   (lambda ($1 . $rest) (reverse $1))
   ;; record-spec-0 => "product" "record" ident ":" type-name
   (lambda ($5 $4 $3 $2 $1 . $rest) (list $5 $3 'prod-recd))
   ;; record-spec-1 => record-spec-0
   (lambda ($1 . $rest) $1)
   ;; record-spec-1 => record-spec-0 "array"
   (lambda ($2 $1 . $rest) (cons '(array) $1))
   ;; record-spec-2 => record-spec-1
   (lambda ($1 . $rest) $1)
   ;; record-spec-2 => record-spec-1 "id" expr
   (lambda ($3 $2 $1 . $rest) (cons '(id ,$3) $1))
   ;; prod-cont-spec => cont-spec-2
   (lambda ($1 . $rest) $1)
   ;; cont-spec-0 => "product" "container" ident
   (lambda ($3 $2 $1 . $rest) (list $3 'prod-cont))
   ;; cont-spec-1 => cont-spec-0
   (lambda ($1 . $rest) $1)
   ;; cont-spec-1 => cont-spec-0 "id" expr
   (lambda ($3 $2 $1 . $rest) (cons '(id ,$3) $1))
   ;; cont-spec-2 => cont-spec-1
   (lambda ($1 . $rest) $1)
   ;; cont-spec-2 => cont-spec-1 "default" "priority" expr
   (lambda ($4 $3 $2 $1 . $rest) (cons `(def-prio ,$4) $1))
   ;; comp-inst-defn => comp-inst-7
   (lambda ($1 . $rest) (reverse $1))
   ;; comp-inst-0 => "instance" ident ":" qual-ident "base" "id" expr
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) (list `(id ,$7) $4 $2 'instance))
   ;; comp-inst-1 => comp-inst-0
   (lambda ($1 . $rest) $1)
   ;; comp-inst-1 => comp-inst-0 "type" string
   (lambda ($3 $2 $1 . $rest) (cons `(type ,$3) $1))
   ;; comp-inst-2 => comp-inst-1
   (lambda ($1 . $rest) $1)
   ;; comp-inst-2 => comp-inst-1 "at" string
   (lambda ($3 $2 $1 . $rest) (cons `(type ,$3) $1))
   ;; comp-inst-3 => comp-inst-2
   (lambda ($1 . $rest) $1)
   ;; comp-inst-3 => comp-inst-2 "queue" "size" expr
   (lambda ($4 $3 $2 $1 . $rest) (cons `(qsiz ,$4) $1))
   ;; comp-inst-4 => comp-inst-3
   (lambda ($1 . $rest) $1)
   ;; comp-inst-4 => comp-inst-3 "stack" "size" expr
   (lambda ($4 $3 $2 $1 . $rest) (cons `(stksiz ,$4) $1))
   ;; comp-inst-5 => comp-inst-4
   (lambda ($1 . $rest) $1)
   ;; comp-inst-5 => comp-inst-4 "priority" expr
   (lambda ($3 $2 $1 . $rest) (cons `(prio ,$3) $1))
   ;; comp-inst-6 => comp-inst-5
   (lambda ($1 . $rest) $1)
   ;; comp-inst-6 => comp-inst-5 "cpu" expr
   (lambda ($3 $2 $1 . $rest) (cons `(cpu ,$3) $1))
   ;; comp-inst-7 => comp-inst-6
   (lambda ($1 . $rest) $1)
   ;; comp-inst-7 => comp-inst-6 "{" string "}"
   (lambda ($4 $3 $2 $1 . $rest) (cons `(code ,$3) $1))
   ;; topology-defn => "topology" ident "{" topo-mem-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(topology-defn ,$2 ,(seq->elt $4)))
   ;; topo-mem-seq => 
   (lambda $rest (make-seq))
   ;; topo-mem-seq => topo-mem
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; topo-mem-seq => include-spec
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; topo-mem-seq => topo-mem mem-sep topo-mem-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; topo-mem-seq => include-spec mem-sep topo-mem-seq
   (lambda ($3 $2 $1 . $rest) $3)
   ;; topo-mem => lone-anno
   (lambda ($1 . $rest) $1)
   ;; topo-mem => comp-inst-spec
   (lambda ($1 . $rest) $1)
   ;; topo-mem => conn-graph-spec
   (lambda ($1 . $rest) $1)
   ;; topo-mem => tlm-pktset-spec
   (lambda ($1 . $rest) $1)
   ;; topo-mem => "import" qual-ident
   (lambda ($2 $1 . $rest) `(import ,$2))
   ;; comp-inst-spec => "instance" ident
   (lambda ($2 $1 . $rest) `(comp-inst ,$2))
   ;; comp-inst-spec => "private" "instance" ident
   (lambda ($3 $2 $1 . $rest) `(comp-priv-inst ,$3))
   ;; conn-graph-spec => "connections" ident "{" conn-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(connections ,$2 ,(seq->elt $4)))
   ;; conn-graph-spec => pattern-kind "connections" "instance" qual-ident
   (lambda ($4 $3 $2 $1 . $rest) `(connections-inst ,$4 (kind ,$1)))
   ;; conn-graph-spec => pattern-kind "connections" "instance" qual-ident "...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(connections-inst ,$4 (kind ,$1) ,(seq->elt $6)))
   ;; pattern-kind => "command"
   (lambda ($1 . $rest) $1)
   ;; pattern-kind => "event"
   (lambda ($1 . $rest) $1)
   ;; pattern-kind => "health"
   (lambda ($1 . $rest) $1)
   ;; pattern-kind => "param"
   (lambda ($1 . $rest) $1)
   ;; pattern-kind => "telemetry"
   (lambda ($1 . $rest) $1)
   ;; pattern-kind => "time"
   (lambda ($1 . $rest) $1)
   ;; pattern-kind => "text" "event"
   (lambda ($2 $1 . $rest) "text-event")
   ;; conn-seq => 
   (lambda $rest (make-seq))
   ;; conn-seq => connection
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; conn-seq => connection elt-sep conn-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; connection => conn-from "->" conn-to
   (lambda ($3 $2 $1 . $rest) `(conn ,$1 ,$3))
   ;; connection => "unmatched" conn-from "->" conn-to
   (lambda ($4 $3 $2 $1 . $rest) `(unmatched-conn ,$1 ,$3))
   ;; conn-from => qual-ident
   (lambda ($1 . $rest) `(from ,$1))
   ;; conn-from => qual-ident "[" expr "]"
   (lambda ($4 $3 $2 $1 . $rest) `(from ,$1 ,$3))
   ;; conn-to => qual-ident
   (lambda ($1 . $rest) `(to ,$1))
   ;; conn-to => qual-ident "[" expr "]"
   (lambda ($4 $3 $2 $1 . $rest) `(to ,$1 ,$3))
   ;; tlm-pktset-spec => "telemetry" "packets" ident "{" tlm-pktgrp-mem-seq...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) `(tlm-packets ,$3 ,(seq->elt $4)))
   ;; tlm-pktset-spec => "telemetry" "packets" ident "{" tlm-pktgrp-mem-seq...
   (lambda ($10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(tlm-packets ,$3 ,(seq->elt $5) (omit ,@(sx-tail (seq->elt $9)))))
   ;; tlm-pktgrp-mem-seq => 
   (lambda $rest (make-seq))
   ;; tlm-pktgrp-mem-seq => tlm-pkt-spec
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; tlm-pktgrp-mem-seq => include-spec
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; tlm-pktgrp-mem-seq => tlm-pkt-spec elt-sep tlm-pktgrp-mem-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; tlm-pktgrp-mem-seq => include-spec elt-sep tlm-pktgrp-mem-seq
   (lambda ($3 $2 $1 . $rest) $3)
   ;; tlm-pkt-spec => "packet" ident "group" expr "{" tlm-pkt-mem-seq "}"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(packet ,$2 (group ,$4) (seq->elt $6)))
   ;; tlm-pkt-spec => "packet" ident "group" expr "id" expr "{" tlm-pkt-mem...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(packet ,$2 (group ,$4) (id ,$6) (seq->elt $8)))
   ;; tlm-pkt-mem-seq => 
   (lambda $rest (make-seq))
   ;; tlm-pkt-mem-seq => qual-ident
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; tlm-pkt-mem-seq => include-spec
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; tlm-pkt-mem-seq => qual-ident elt-sep tlm-pkt-mem-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; tlm-pkt-mem-seq => include-spec elt-sep tlm-pkt-mem-seq
   (lambda ($3 $2 $1 . $rest) $3)
   ;; tlm-chan-id-seq => qual-ident-seq
   (lambda ($1 . $rest) $1)
   ;; param-list => param-list-1
   (lambda ($1 . $rest) (seq->elt $1))
   ;; param-list-1 => 
   (lambda $rest (make-seq))
   ;; param-list-1 => formal-param
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; param-list-1 => formal-param elt-sep param-list-1
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; formal-param => formal-param-1
   (lambda ($1 . $rest) $1)
   ;; formal-param => formal-param-1 code-anno
   (lambda ($2 $1 . $rest) (cons* (sx-tag $1) `(@ (anno ,$2)) (sx-tail $1)))
   ;; formal-param-1 => ident ":" type-name
   (lambda ($3 $2 $1 . $rest) `(param ,$1 ,$3))
   ;; formal-param-1 => "ref" ident ":" type-name
   (lambda ($4 $3 $2 $1 . $rest) `(ref-param ,$2 ,$4))
   ;; queue-full-beh => queue-full-beh-1
   (lambda ($1 . $rest) `(q-full-beh ,$1))
   ;; queue-full-beh-1 => "assert"
   (lambda ($1 . $rest) $1)
   ;; queue-full-beh-1 => "block"
   (lambda ($1 . $rest) $1)
   ;; queue-full-beh-1 => "drop"
   (lambda ($1 . $rest) $1)
   ;; queue-full-beh-1 => "hook"
   (lambda ($1 . $rest) $1)
   ;; loc-spec => "locate" "instance" qual-ident "at" string
   (lambda ($5 $4 $3 $2 $1 . $rest) `(loc-inst ,$3 (at ,$5)))
   ;; loc-spec => "locate" "component" qual-ident "at" string
   (lambda ($5 $4 $3 $2 $1 . $rest) `(loc-comp ,$3 (at ,$5)))
   ;; loc-spec => "locate" "constant" qual-ident "at" string
   (lambda ($5 $4 $3 $2 $1 . $rest) `(loc-const ,$3 (at ,$5)))
   ;; loc-spec => "locate" "port" qual-ident "at" string
   (lambda ($5 $4 $3 $2 $1 . $rest) `(loc-port ,$3 (at ,$5)))
   ;; loc-spec => "locate" "state" "machine" qual-ident "at" string
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) `(loc-stmach ,$3 (at ,$6)))
   ;; loc-spec => "locate" "topology" qual-ident "at" string
   (lambda ($5 $4 $3 $2 $1 . $rest) `(loc-topo ,$3 (at ,$5)))
   ;; loc-spec => "locate" "type" qual-ident "at" string
   (lambda ($5 $4 $3 $2 $1 . $rest) `(loc-type ,$3 (at ,$5)))
   ;; port-match-spec => "match" ident "with" ident
   (lambda ($4 $3 $2 $1 . $rest) `(match ,$2 ,$4))
   ;; expr => add-expr
   (lambda ($1 . $rest) `(expr ,$1))
   ;; add-expr => mul-expr
   (lambda ($1 . $rest) $1)
   ;; add-expr => add-expr "+" mul-expr
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; add-expr => add-expr "-" mul-expr
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; mul-expr => unary-expr
   (lambda ($1 . $rest) $1)
   ;; mul-expr => mul-expr "*" unary-expr
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; mul-expr => mul-expr "/" unary-expr
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; unary-expr => prim-expr
   (lambda ($1 . $rest) $1)
   ;; unary-expr => "-" unary-expr
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; prim-expr => qual-ident
   (lambda ($1 . $rest) $1)
   ;; prim-expr => number
   (lambda ($1 . $rest) $1)
   ;; prim-expr => string
   (lambda ($1 . $rest) $1)
   ;; prim-expr => "[" expr-seq "]"
   (lambda ($3 $2 $1 . $rest) `(array-val ,@(sx-tail (seq->elt $2))))
   ;; prim-expr => "{" struct-elt-seq "}"
   (lambda ($3 $2 $1 . $rest) `(struct-val ,@(sx-tail (seq->elt $2))))
   ;; prim-expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; expr-seq => expr
   (lambda ($1 . $rest) (make-seq))
   ;; expr-seq => expr elt-sep expr-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; struct-elt-seq => 
   (lambda $rest (make-seq))
   ;; struct-elt-seq => struct-elt
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; struct-elt-seq => struct-elt elt-sep struct-elt-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; struct-elt => ident "=" expr
   (lambda ($3 $2 $1 . $rest) `(bind-struct-elt ,$1 ,$3))
   ;; number => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; number => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; ident => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ;; lone-anno => '$lone-anno
   (lambda ($1 . $rest) `(lone-anno ,$1))
   ;; code-anno => code-anno-list
   (lambda ($1 . $rest) (string-join (reverse $1) "\n"))
   ;; code-anno-list => '$code-anno
   (lambda ($1 . $rest) (list $1))
   ;; qual-ident => qual-ident-1
   (lambda ($1 . $rest) (reverse $1))
   ;; qual-ident-1 => ident
   (lambda ($1 . $rest) (list (sx-ref $1 1) 'qual-ident))
   ;; qual-ident-1 => qual-ident-1 "." ident
   (lambda ($3 $2 $1 . $rest) (cons (sx-ref $3 1) $1))
   ;; qual-ident-seq => 
   (lambda $rest (make-seq))
   ;; qual-ident-seq => qual-ident
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; qual-ident-seq => qual-ident elt-sep qual-ident-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; index => "[" expr "]"
   (lambda ($3 $2 $1 . $rest) `(index ,$2))
   ;; type-name => qual-ident
   (lambda ($1 . $rest) `(type-name ,$1))
   ;; type-name => "I8"
   (lambda ($1 . $rest) `(type-name (fixed ,$1)))
   ;; type-name => "U8"
   (lambda ($1 . $rest) `(type-name (fixed ,$1)))
   ;; type-name => "I16"
   (lambda ($1 . $rest) `(type-name (fixed ,$1)))
   ;; type-name => "U16"
   (lambda ($1 . $rest) `(type-name (fixed ,$1)))
   ;; type-name => "I32"
   (lambda ($1 . $rest) `(type-name (fixed ,$1)))
   ;; type-name => "U32"
   (lambda ($1 . $rest) `(type-name (fixed ,$1)))
   ;; type-name => "I64"
   (lambda ($1 . $rest) `(type-name (fixed ,$1)))
   ;; type-name => "U64"
   (lambda ($1 . $rest) `(type-name (fixed ,$1)))
   ;; type-name => "F32"
   (lambda ($1 . $rest) `(type-name (float ,$1)))
   ;; type-name => "F64"
   (lambda ($1 . $rest) `(type-name (float ,$1)))
   ;; type-name => "bool"
   (lambda ($1 . $rest) `(type-name (bool ,$1)))
   ;; type-name => "string"
   (lambda ($1 . $rest) `(type-name (string ,$1)))
   ;; type-name => "string" "size" expr
   (lambda ($3 $2 $1 . $rest) `(type-name (string ,$1 (size ,$3))))
   ;; stmach-inst => stmach-inst-2
   (lambda ($1 . $rest) (reverse $1))
   ;; stmach-inst => stmach-inst-2 code-anno
   (lambda ($2 $1 . $rest)
     (let (($1 (reverse $1))) (cons* (car $1) `(@ (anno ,$2)) (cdr $1))))
   ;; stmach-inst-0 => "state" "machine" "instance" ident ":" qual-ident
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) (list $5 $4 'stmach-inst))
   ;; stmach-inst-1 => stmach-inst-0
   (lambda ($1 . $rest) $1)
   ;; stmach-inst-1 => stmach-inst-0 "priority" expr
   (lambda ($3 $2 $1 . $rest) (cons `(prio ,$3) $1))
   ;; stmach-inst-2 => stmach-inst-1
   (lambda ($1 . $rest) $1)
   ;; stmach-inst-2 => stmach-inst-1 queue-full-beh
   (lambda ($2 $1 . $rest) (cons $2 $1))
   ;; stmach-defn => "state" "machine" ident
   (lambda ($3 $2 $1 . $rest) `(stmach-defn ,$3))
   ;; stmach-defn => "state" "machine" ident "{" stmach-mem-seq "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) `(stmach-defn ,$3 ,(seq->elt $5)))
   ;; stmach-mem-seq => 
   (lambda $rest (make-seq))
   ;; stmach-mem-seq => stmach-mem
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; stmach-mem-seq => stmach-mem mem-sep stmach-mem-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; stmach-mem => lone-anno
   (lambda ($1 . $rest) $1)
   ;; stmach-mem => "choice" ident "{" "if" ident trans-expr "else" trans-e...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest) `(choice ,$2 ,$5 ,$6 ,$7))
   ;; stmach-mem => "action" ident
   (lambda ($2 $1 . $rest) `(action ,$2))
   ;; stmach-mem => "action" ident ":" type-name
   (lambda ($4 $3 $2 $1 . $rest) `(action ,$2 ,$4))
   ;; stmach-mem => "guard" ident
   (lambda ($2 $1 . $rest) `(guard ,$2))
   ;; stmach-mem => "guard" ident ":" type-name
   (lambda ($4 $3 $2 $1 . $rest) `(guard ,$2 ,$4))
   ;; stmach-mem => "signal" ident
   (lambda ($2 $1 . $rest) `(signal ,$2))
   ;; stmach-mem => "signal" ident ":" type-name
   (lambda ($4 $3 $2 $1 . $rest) `(signal ,$2 ,$4))
   ;; stmach-mem => "initial" trans-expr
   (lambda ($2 $1 . $rest) `(initial ,$2))
   ;; stmach-mem => state-defn
   (lambda ($1 . $rest) $1)
   ;; state-defn => "state" ident
   (lambda ($2 $1 . $rest) `(state-defn ,$2))
   ;; state-defn => "state" ident "{" state-defn-mem-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(state-defn ,$2 ,(seq->elt $4)))
   ;; state-defn-mem-seq => 
   (lambda $rest (make-seq))
   ;; state-defn-mem-seq => state-defn-mem
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; state-defn-mem-seq => state-defn-mem mem-sep state-defn-mem-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; state-defn-mem => lone-anno
   (lambda ($1 . $rest) $1)
   ;; state-defn-mem => "initial" trans-expr
   (lambda ($2 $1 . $rest) `(intial ,$2))
   ;; state-defn-mem => "choice" ident "{" "if" ident trans-expr "else" tra...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest) `(choice ,$2 ,$5 ,$6 ,$7))
   ;; state-defn-mem => state-defn
   (lambda ($1 . $rest) $1)
   ;; state-defn-mem => state-trans-spec
   (lambda ($1 . $rest) $1)
   ;; state-defn-mem => "entry" do-expr
   (lambda ($2 $1 . $rest) `(entry ,$2))
   ;; state-defn-mem => "exit" do-expr
   (lambda ($2 $1 . $rest) `(exit ,$2))
   ;; state-trans-spec => st-tran-spec-2
   (lambda ($1 . $rest) $1)
   ;; st-tran-spec-0 => "on" ident
   (lambda ($2 $1 . $rest) $1)
   ;; st-tran-spec-1 => st-tran-spec-0
   (lambda ($1 . $rest) $1)
   ;; st-tran-spec-1 => st-tran-spec-0 "if" ident
   (lambda ($3 $2 $1 . $rest) $1)
   ;; st-tran-spec-2 => st-tran-spec-1 trans-or-do
   (lambda ($2 $1 . $rest) $1)
   ;; trans-expr => trans-expr-1
   (lambda ($1 . $rest) $1)
   ;; trans-expr-0 => "enter" qual-ident
   (lambda ($2 $1 . $rest) $1)
   ;; trans-expr-1 => trans-expr-0
   (lambda ($1 . $rest) $1)
   ;; trans-expr-1 => do-expr trans-expr-0
   (lambda ($2 $1 . $rest) $1)
   ;; do-expr => "do" "{" action-seq "}"
   (lambda ($4 $3 $2 $1 . $rest) `(do-expr ,(seq->elt $3)))
   ;; action-seq => 
   (lambda $rest (make-seq))
   ;; action-seq => ident
   (lambda ($1 . $rest) (seq-insert (make-seq) $1))
   ;; action-seq => ident elt-sep action-seq
   (lambda ($3 $2 $1 . $rest) (seq-insert $3 $1))
   ;; trans-or-do => trans-expr
   (lambda ($1 . $rest) $1)
   ;; trans-or-do => do-expr
   (lambda ($1 . $rest) $1)
   ))

;;; end tables
