;; fpp-act.scm

;; Copyright 2025 Matthew Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING included with the this distribution.

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
   ;; include-spec => "include" string
   (lambda ($2 $1 . $rest) `(include ,$2))
   ;; translation-unit => module-mem-seq
   (lambda ($1 . $rest) `(trans-unit ,(tl->list $1)))
   ;; module-mem-seq => 
   (lambda $rest (make-tl 'module-mem-set))
   ;; module-mem-seq => mod-mem mem-sep module-mem-seq
   (lambda ($3 $2 $1 . $rest) (tl-insert $3 $1))
   ;; mod-mem => include-spec
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
   ;; module-defn => "module" ident "{" module-mem-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(module-defn ,$2 ,(tl->list $4)))
   ;; const-defn => "constant" ident "=" expr
   (lambda ($4 $3 $2 $1 . $rest) `(const-defn ,$2 ,$4))
   ;; abs-type-defn => "type" ident
   (lambda ($2 $1 . $rest) `(type-defn ,$2))
   ;; array-defn => "array" ident "=" index ident
   (lambda ($5 $4 $3 $2 $1 . $rest) `(array-defn ,$3 ,$2))
   ;; enum-defn => enum-defn-3
   (lambda ($1 . $rest) (reverse $1))
   ;; enum-defn-0 => "enum" ident
   (lambda ($2 $1 . $rest) (list $2 'enum-defn))
   ;; enum-defn-1 => enum-defn-0
   (lambda ($1 . $rest) $1)
   ;; enum-defn-1 => enum-defn-0 ":" type-name
   (lambda ($3 $2 $1 . $rest) (cons $3 $1))
   ;; enum-defn-2 => enum-defn-1 "{" enum-mem-seq "}"
   (lambda ($4 $3 $2 $1 . $rest) (cons (tl->list $3) $1))
   ;; enum-defn-3 => enum-defn-2
   (lambda ($1 . $rest) $1)
   ;; enum-defn-3 => enum-defn-2 "default" expr
   (lambda ($3 $2 $1 . $rest) (cons `(default ,$3) $1))
   ;; enum-mem-seq => 
   (lambda $rest (make-tl 'enum-mem-seq))
   ;; enum-mem-seq => enum-mem elt-sep enum-mem-seq
   (lambda ($3 $2 $1 . $rest) (tl-insert $3 $1))
   ;; enum-mem => ident
   (lambda ($1 . $rest) `(enum $1))
   ;; enum-mem => ident "=" expr
   (lambda ($3 $2 $1 . $rest) `(enum $1 $3))
   ;; struct-defn => struct-defn-1
   (lambda ($1 . $rest) (reverse $1))
   ;; struct-defn-0 => "struct" ident "{" struct-mem-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) (list (tl->list $4) $2 'struct-defn))
   ;; struct-defn-1 => struct-defn-0
   (lambda ($1 . $rest) $1)
   ;; struct-defn-1 => struct-defn-0 "default" expr
   (lambda ($3 $2 $1 . $rest) (cons `(default ,$3) $1))
   ;; struct-mem-seq => 
   (lambda $rest (make-tl 'mem-seq))
   ;; struct-mem-seq => struct-mem mem-sep struct-mem-seq
   (lambda ($3 $2 $1 . $rest) (tl-insert $3 $1))
   ;; struct-mem => struct-mem-3
   (lambda ($1 . $rest) (reverse $1))
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
   ;; port-defn => port-defn-2
   (lambda ($1 . $rest) reverse $1)
   ;; port-defn-0 => "port" ident
   (lambda ($2 $1 . $rest) (list ident 'port-defn))
   ;; port-defn-1 => port-defn-0
   (lambda ($1 . $rest) $1)
   ;; port-defn-1 => port-defn-0 "(" param-list ")"
   (lambda ($4 $3 $2 $1 . $rest) (cons $3 $1))
   ;; port-defn-2 => port-defn-1
   (lambda ($1 . $rest) $1)
   ;; port-defn-2 => port-defn-1 "->" type-name
   (lambda ($3 $2 $1 . $rest) (cons $3 $1))
   ;; component-defn => comp-kind ident "{" comp-mem-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(comp-defn (@ (kind ,$1)) ,$2 ,(reverse $4)))
   ;; comp-kind => "active"
   (lambda ($1 . $rest) $1)
   ;; comp-kind => "passive"
   (lambda ($1 . $rest) $1)
   ;; comp-kind => "queued"
   (lambda ($1 . $rest) $1)
   ;; comp-mem-seq => 
   (lambda $rest (make-tl 'mem-seq))
   ;; comp-mem-seq => comp-mem mem-sep comp-mem-seq
   (lambda ($3 $2 $1 . $rest) (tl-insert $3 $1))
   ;; comp-mem => include-spec
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
   ;; port-inst => gen-port-inst-4
   (lambda ($1 . $rest) $1)
   ;; port-inst => spc-port-inst-3
   (lambda ($1 . $rest) $1)
   ;; gen-port-inst-0 => input-port-kind "input" "port" ident ":"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; gen-port-inst-0 => "output" "port" ident ":"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; gen-port-inst-1 => gen-port-inst-0
   (lambda ($1 . $rest) $1)
   ;; gen-port-inst-1 => gen-port-inst-0 expr
   (lambda ($2 $1 . $rest) $1)
   ;; gen-port-inst-2 => gen-port-inst-1 qual-ident
   (lambda ($2 $1 . $rest) $1)
   ;; gen-port-inst-2 => gen-port-inst-1 "serial"
   (lambda ($2 $1 . $rest) $1)
   ;; gen-port-inst-3 => gen-port-inst-2
   (lambda ($1 . $rest) $1)
   ;; gen-port-inst-3 => gen-port-inst-2 "priority" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; gen-port-inst-4 => gen-port-inst-3
   (lambda ($1 . $rest) $1)
   ;; gen-port-inst-4 => gen-port-inst-3 queue-full-beh
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-inst-0 => spc-port-kind "port" ident ":"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; spc-port-inst-1 => spc-port-inst-0
   (lambda ($1 . $rest) $1)
   ;; spc-port-inst-1 => input-port-kind spc-port-inst-0
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-inst-2 => spc-port-inst-1
   (lambda ($1 . $rest) $1)
   ;; spc-port-inst-2 => spc-port-inst-1 "priority" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; spc-port-inst-3 => spc-port-inst-2
   (lambda ($1 . $rest) $1)
   ;; spc-port-inst-3 => spc-port-inst-2 queue-full-beh
   (lambda ($2 $1 . $rest) $1)
   ;; int-port-spec => int-port-defn-3
   (lambda ($1 . $rest) $1)
   ;; int-port-defn-0 => "internal" "port" ident
   (lambda ($3 $2 $1 . $rest) $1)
   ;; int-port-defn-1 => int-port-defn-0
   (lambda ($1 . $rest) $1)
   ;; int-port-defn-1 => int-port-defn-0 "(" param-list ")"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; int-port-defn-2 => int-port-defn-1
   (lambda ($1 . $rest) $1)
   ;; int-port-defn-2 => int-port-defn-1 "priority" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; int-port-defn-3 => int-port-defn-2
   (lambda ($1 . $rest) $1)
   ;; int-port-defn-3 => int-port-defn-2 queue-full-beh
   (lambda ($2 $1 . $rest) $1)
   ;; command-spec => cmd-spec-4
   (lambda ($1 . $rest) $1)
   ;; cmd-spec-0 => input-port-kind "command" ident
   (lambda ($3 $2 $1 . $rest) $1)
   ;; cmd-spec-1 => cmd-spec-0
   (lambda ($1 . $rest) $1)
   ;; cmd-spec-1 => cmd-spec-0 "(" param-list ")"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; cmd-spec-2 => cmd-spec-1
   (lambda ($1 . $rest) $1)
   ;; cmd-spec-2 => cmd-spec-1 "opcode" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; cmd-spec-3 => cmd-spec-2
   (lambda ($1 . $rest) $1)
   ;; cmd-spec-3 => cmd-spec-2 "priority" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; cmd-spec-4 => cmd-spec-3
   (lambda ($1 . $rest) $1)
   ;; cmd-spec-4 => cmd-spec-3 queue-full-beh
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-kind => "command" "recv"
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-kind => "command" "reg"
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-kind => "command" "resp"
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-kind => "event"
   (lambda ($1 . $rest) $1)
   ;; spc-port-kind => "param" "get"
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-kind => "param" "set"
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-kind => "product" "get"
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-kind => "product" "recv"
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-kind => "product" "request"
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-kind => "product" "send"
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-kind => "telemetry"
   (lambda ($1 . $rest) $1)
   ;; spc-port-kind => "text" "event"
   (lambda ($2 $1 . $rest) $1)
   ;; spc-port-kind => "time" "get"
   (lambda ($2 $1 . $rest) $1)
   ;; input-port-kind => "async"
   (lambda ($1 . $rest) 'async)
   ;; input-port-kind => "guarded"
   (lambda ($1 . $rest) 'guarded)
   ;; input-port-kind => "sync"
   (lambda ($1 . $rest) 'async)
   ;; event-spec => event-spec-5
   (lambda ($1 . $rest) $1)
   ;; event-spec-0 => "event" ident
   (lambda ($2 $1 . $rest) $1)
   ;; event-spec-1 => event-spec-0
   (lambda ($1 . $rest) $1)
   ;; event-spec-1 => event-spec-0 "(" param-list ")"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; event-spec-2 => event-spec-1 "severity" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; event-spec-3 => event-spec-2
   (lambda ($1 . $rest) $1)
   ;; event-spec-3 => event-spec-2 "id" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; event-spec-4 => event-spec-3 "format" string
   (lambda ($3 $2 $1 . $rest) $1)
   ;; event-spec-5 => event-spec-4
   (lambda ($1 . $rest) $1)
   ;; event-spec-5 => event-spec-4 "throttle" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; param-spec => param-spec-4
   (lambda ($1 . $rest) $1)
   ;; param-spec-0 => "param" ident ":" type-name
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; param-spec-1 => param-spec-0
   (lambda ($1 . $rest) $1)
   ;; param-spec-1 => param-spec-0 "default" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; param-spec-2 => param-spec-1
   (lambda ($1 . $rest) $1)
   ;; param-spec-2 => param-spec-1 "id" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; param-spec-3 => param-spec-2
   (lambda ($1 . $rest) $1)
   ;; param-spec-3 => param-spec-2 "set" "opcode" expr
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; param-spec-4 => param-spec-3
   (lambda ($1 . $rest) $1)
   ;; param-spec-4 => param-spec-3 "save" "opcode" expr
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; tlm-chan-spec => tlm-chan-5
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-0 => "telemetry" ident ":" type-name
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; tlm-chan-1 => tlm-chan-0
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-1 => tlm-chan-0 "id" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; tlm-chan-2 => tlm-chan-1
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-2 => tlm-chan-1 "update" tlm-update
   (lambda ($3 $2 $1 . $rest) $1)
   ;; tlm-chan-3 => tlm-chan-2
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-3 => tlm-chan-2 "format" string
   (lambda ($3 $2 $1 . $rest) $1)
   ;; tlm-chan-4 => tlm-chan-3
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-4 => tlm-chan-3 "low" "{" tlm-lim-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; tlm-chan-5 => tlm-chan-4
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-5 => tlm-chan-4 "high" "{" tlm-lim-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; tlm-update => "always"
   (lambda ($1 . $rest) 'always)
   ;; tlm-update => "on" "change"
   (lambda ($2 $1 . $rest) 'on-change)
   ;; tlm-lim-seq => tlm-lim
   (lambda ($1 . $rest) $1)
   ;; tlm-lim-seq => tlm-lim-seq elt-sep tlm-lim
   (lambda ($3 $2 $1 . $rest) $1)
   ;; tlm-lim => "red" expr
   (lambda ($2 $1 . $rest) $1)
   ;; tlm-lim => "orange" expr
   (lambda ($2 $1 . $rest) $1)
   ;; tlm-lim => "yellow" expr
   (lambda ($2 $1 . $rest) $1)
   ;; record-spec => record-spec-2
   (lambda ($1 . $rest) $1)
   ;; record-spec-0 => "product" "record" ident ":" type-name
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; record-spec-1 => record-spec-0
   (lambda ($1 . $rest) $1)
   ;; record-spec-1 => record-spec-0 "array"
   (lambda ($2 $1 . $rest) $1)
   ;; record-spec-2 => record-spec-1
   (lambda ($1 . $rest) $1)
   ;; record-spec-2 => record-spec-1 "id" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; prod-cont-spec => cont-spec-2
   (lambda ($1 . $rest) $1)
   ;; cont-spec-0 => "product" "container" ident
   (lambda ($3 $2 $1 . $rest) $1)
   ;; cont-spec-1 => cont-spec-0
   (lambda ($1 . $rest) $1)
   ;; cont-spec-1 => cont-spec-0 "id" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; cont-spec-2 => cont-spec-1
   (lambda ($1 . $rest) $1)
   ;; cont-spec-2 => cont-spec-1 "default" "priority" expr
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; comp-inst-defn => comp-inst-7
   (lambda ($1 . $rest) $1)
   ;; comp-inst-0 => "instance" ident ":" qual-ident "base" "id" expr
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) `(comp-inst-defn ,$2 ,$4 ,$7))
   ;; comp-inst-1 => comp-inst-0
   (lambda ($1 . $rest) $1)
   ;; comp-inst-1 => comp-inst-0 "type" string
   (lambda ($3 $2 $1 . $rest) (append $1 (list `(type ,$3))))
   ;; comp-inst-2 => comp-inst-1
   (lambda ($1 . $rest) $1)
   ;; comp-inst-2 => comp-inst-1 "at" string
   (lambda ($3 $2 $1 . $rest) (append $1 (list `(type ,$3))))
   ;; comp-inst-3 => comp-inst-2
   (lambda ($1 . $rest) $1)
   ;; comp-inst-3 => comp-inst-2 "queue" "size" expr
   (lambda ($4 $3 $2 $1 . $rest) (append $1 (list `(qsiz ,$4))))
   ;; comp-inst-4 => comp-inst-3
   (lambda ($1 . $rest) $1)
   ;; comp-inst-4 => comp-inst-3 "stack" "size" expr
   (lambda ($4 $3 $2 $1 . $rest) (append $1 (list `(stksiz ,$4))))
   ;; comp-inst-5 => comp-inst-4
   (lambda ($1 . $rest) $1)
   ;; comp-inst-5 => comp-inst-4 "priority" expr
   (lambda ($3 $2 $1 . $rest) (append $1 (list `(prio ,$3))))
   ;; comp-inst-6 => comp-inst-5
   (lambda ($1 . $rest) $1)
   ;; comp-inst-6 => comp-inst-5 "cpu" expr
   (lambda ($3 $2 $1 . $rest) (append $1 (list `(cpu ,$3))))
   ;; comp-inst-7 => comp-inst-6
   (lambda ($1 . $rest) $1)
   ;; comp-inst-7 => comp-inst-6 "{" string "}"
   (lambda ($4 $3 $2 $1 . $rest) (append $1 (list `(code ,$3))))
   ;; topology-defn => "topology" ident "{" topo-mem-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; topo-mem-seq => 
   (lambda $rest (list))
   ;; topo-mem-seq => topo-mem mem-sep topo-mem-seq
   (lambda ($3 $2 $1 . $rest) $1)
   ;; topo-mem => comp-inst-spec
   (lambda ($1 . $rest) $1)
   ;; topo-mem => conn-graph-spec
   (lambda ($1 . $rest) $1)
   ;; topo-mem => tlm-pktset-spec
   (lambda ($1 . $rest) $1)
   ;; topo-mem => "import" qual-ident
   (lambda ($2 $1 . $rest) $1)
   ;; topo-mem => include-spec
   (lambda ($1 . $rest) $1)
   ;; comp-inst-spec => "instance" ident
   (lambda ($2 $1 . $rest) $1)
   ;; comp-inst-spec => "private" "instance" ident
   (lambda ($3 $2 $1 . $rest) $1)
   ;; conn-graph-spec => "connections" ident "{" conn-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; conn-graph-spec => pattern-kind "connections" "instance" qual-ident
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; conn-graph-spec => pattern-kind "connections" "instance" qual-ident "...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; pattern-kind => "command"
   (lambda ($1 . $rest) 'command)
   ;; pattern-kind => "event"
   (lambda ($1 . $rest) 'event)
   ;; pattern-kind => "health"
   (lambda ($1 . $rest) 'health)
   ;; pattern-kind => "param"
   (lambda ($1 . $rest) 'command)
   ;; pattern-kind => "telemetry"
   (lambda ($1 . $rest) 'command)
   ;; pattern-kind => "text" "event"
   (lambda ($2 $1 . $rest) 'text-event)
   ;; pattern-kind => "time"
   (lambda ($1 . $rest) 'time)
   ;; conn-seq => 
   (lambda $rest (list))
   ;; conn-seq => connection elt-sep conn-seq
   (lambda ($3 $2 $1 . $rest) $1)
   ;; connection => connection-4
   (lambda ($1 . $rest) $1)
   ;; connection-0 => qual-ident
   (lambda ($1 . $rest) $1)
   ;; connection-1 => connection-0
   (lambda ($1 . $rest) $1)
   ;; connection-1 => "unmatched" connection-0
   (lambda ($2 $1 . $rest) $1)
   ;; connection-2 => connection-1
   (lambda ($1 . $rest) $1)
   ;; connection-2 => connection-1 "[" expr "]"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; connection-3 => connection-2 "->" qual-ident
   (lambda ($3 $2 $1 . $rest) $1)
   ;; connection-4 => connection-3
   (lambda ($1 . $rest) $1)
   ;; connection-4 => connection-3 "[" expr "]"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; tlm-pktset-spec => tlm-pktset-spec-1
   (lambda ($1 . $rest) $1)
   ;; tlm-pktset-spec-0 => "telemetry" "packets" ident "{" tlm-pktgrp-mem-s...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; tlm-pktset-spec-1 => tlm-pktset-spec-0
   (lambda ($1 . $rest) $1)
   ;; tlm-pktset-spec-1 => tlm-pktset-spec-0 "omit" "{" tlm-chan-id-seq "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; tlm-pktgrp-mem-seq => 
   (lambda $rest (list))
   ;; tlm-pktgrp-mem-seq => tlm-pktgrp-mem elt-sep tlm-pktgrp-mem-seq
   (lambda ($3 $2 $1 . $rest) $1)
   ;; tlm-pktgrp-mem => include-spec
   (lambda ($1 . $rest) $1)
   ;; tlm-pktgrp-mem => tlm-pkt-spec
   (lambda ($1 . $rest) $1)
   ;; tlm-pkt-spec => tlm-pkt-spec-2
   (lambda ($1 . $rest) $1)
   ;; tlm-pkt-spec-0 => "packet" ident
   (lambda ($2 $1 . $rest) $1)
   ;; tlm-pkt-spec-1 => tlm-pkt-spec-0
   (lambda ($1 . $rest) $1)
   ;; tlm-pkt-spec-1 => tlm-pkt-spec-0 "id" expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; tlm-pkt-spec-2 => tlm-pkt-spec-1 "group" expr "{" tlm-pkt-mem-seq "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; tlm-pkt-mem-seq => 
   (lambda $rest (list))
   ;; tlm-pkt-mem-seq => tlm-pkt-mem elt-sep tlm-pkt-mem-seq
   (lambda ($3 $2 $1 . $rest) $1)
   ;; tlm-pkt-mem => include-spec
   (lambda ($1 . $rest) $1)
   ;; tlm-pkt-mem => qual-ident
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-id-seq => qual-ident
   (lambda ($1 . $rest) $1)
   ;; tlm-chan-id-seq => tlm-chan-id-seq elt-sep qual-ident
   (lambda ($3 $2 $1 . $rest) $1)
   ;; param-list => formal-param
   (lambda ($1 . $rest) $1)
   ;; param-list => param-list elt-sep formal-param
   (lambda ($3 $2 $1 . $rest) $1)
   ;; formal-param => ident ":" type-name
   (lambda ($3 $2 $1 . $rest) $1)
   ;; formal-param => "ref" ident ":" type-name
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; queue-full-beh => "assert"
   (lambda ($1 . $rest) 'assert)
   ;; queue-full-beh => "block"
   (lambda ($1 . $rest) 'block)
   ;; queue-full-beh => "drop"
   (lambda ($1 . $rest) 'drop)
   ;; queue-full-beh => "hook"
   (lambda ($1 . $rest) 'hook)
   ;; loc-spec => "locate" "instance" qual-ident "at" string
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; loc-spec => "locate" "component" qual-ident "at" string
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; loc-spec => "locate" "cnstant" qual-ident "at" string
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; loc-spec => "locate" "port" qual-ident "at" string
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; loc-spec => "locate" "state" "machine" qual-ident "at" string
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; loc-spec => "locate" "topology" qual-ident "at" string
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; loc-spec => "locate" "type" qual-ident "at" string
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; port-match-spec => "match" ident "with" ident
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; expr => add-expr
   (lambda ($1 . $rest) $1)
   ;; add-expr => mul-expr
   (lambda ($1 . $rest) $1)
   ;; add-expr => add-expr "+" mul-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; add-expr => add-expr "-" mul-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; mul-expr => unary-expr
   (lambda ($1 . $rest) $1)
   ;; mul-expr => mul-expr "*" unary-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; mul-expr => mul-expr "/" unary-expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; unary-expr => prim-expr
   (lambda ($1 . $rest) $1)
   ;; unary-expr => "-" unary-expr
   (lambda ($2 $1 . $rest) $1)
   ;; prim-expr => qual-ident
   (lambda ($1 . $rest) $1)
   ;; prim-expr => number
   (lambda ($1 . $rest) $1)
   ;; prim-expr => string
   (lambda ($1 . $rest) $1)
   ;; prim-expr => "[" expr-seq "]"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; prim-expr => "{" struct-elt-seq "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; prim-expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; expr-seq => expr
   (lambda ($1 . $rest) $1)
   ;; expr-seq => expr elt-sep expr
   (lambda ($3 $2 $1 . $rest) $1)
   ;; struct-elt-seq => 
   (lambda $rest (list))
   ;; struct-elt-seq => ident "=" expr elt-sep struct-elt-seq
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; number => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; number => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; ident => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ;; qual-ident => qual-ident-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; qual-ident-1 => ident
   (lambda ($1 . $rest) (make-tl 'qual-ident (sx-ref $1 1)))
   ;; qual-ident-1 => qual-ident-1 "." ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 (sx-ref $3 1)))
   ;; qual-ident-seq => qual-ident
   (lambda ($1 . $rest) $1)
   ;; qual-ident-seq => qual-ident-seq elt-sep qual-ident
   (lambda ($3 $2 $1 . $rest) $1)
   ;; index => "[" expr "]"
   (lambda ($3 $2 $1 . $rest) `(index $2))
   ;; type-name => "I8"
   (lambda ($1 . $rest) `(type-name $1))
   ;; type-name => "U8"
   (lambda ($1 . $rest) `(type-name $1))
   ;; type-name => "I16"
   (lambda ($1 . $rest) `(type-name $1))
   ;; type-name => "U16"
   (lambda ($1 . $rest) `(type-name $1))
   ;; type-name => "I32"
   (lambda ($1 . $rest) `(type-name $1))
   ;; type-name => "U32"
   (lambda ($1 . $rest) `(type-name $1))
   ;; type-name => "I64"
   (lambda ($1 . $rest) `(type-name $1))
   ;; type-name => "U64"
   (lambda ($1 . $rest) `(type-name $1))
   ;; type-name => "F32"
   (lambda ($1 . $rest) `(type-name $1))
   ;; type-name => "F64"
   (lambda ($1 . $rest) `(type-name $1))
   ;; type-name => "bool"
   (lambda ($1 . $rest) `(type-name $1))
   ;; type-name => "string"
   (lambda ($1 . $rest) `(type-name $1))
   ;; type-name => "string" "size" expr
   (lambda ($3 $2 $1 . $rest) `(type-name (@ (size ,$3)) $1))
   ))

;;; end tables
