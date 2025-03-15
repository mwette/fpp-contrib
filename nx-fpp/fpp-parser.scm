;; fpp/parser.scm

;; Copyright (C) 2025 Matthew Wette
;; SPDX-License-Identifier: Apache-2.0

(define-module (fpp-parser)
  #:export (parse-fpp read-fpp-file)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util)        ; push-input, pop-input
  #:use-module (nyacc lang sx-util)     ; sx-ref,sx-tail
  #:use-module ((srfi srfi-1) #:select (fold))
  #:declarative? #t)

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

(define (make-seq)
  '())

(define (seq-insert sq item)
  (if (eq? 'lone-anno (car item))
      (if (eq? 'lone-anno (caar sq))
          (cons (cons* 'lone-anno (cadr item) (cdar sq)) (cdr sq))
          (cons item sq))
      (if (and (pair? sq) (eq? 'lone-anno (caar sq)))
          (let ((ae `(anno ,(string-join (cdar sq) "\n"))))
            (cons* item (cons* (caadr sq) `(@ ,ae) (cdadr sq)) (cddr sq)))
          (cons item sq))))

(define (seq->elt sq)
  (cons 'seq
        (if (and (pair? sq) (eq? 'lone-anno (caar sq)))
            (let ((ae `(anno ,(string-join (cdar sq) "\n"))))
              (cons (cons* (caadr sq) `(@ ,ae) (cdadr sq)) (cddr sq)))
            sq)))

;;(export make-seq seq-insert seq->elt)
;;(define-public t-seq (make-seq))
;;(set! t-seq (seq-insert t-seq '(baz "BAZ")))
;;(set! t-seq (seq-insert t-seq '(bar "BAR")))
;;(set! t-seq (seq-insert t-seq '(lone-anno "BAR ANNO")))
;;(set! t-seq (seq-insert t-seq '(lone-anno "Consider:")))
;;(set! t-seq (seq-insert t-seq '(foo "FOO")))

(define (annoverse terms)
  (let ((anno `(@ (anno (car terms)))) (form (reverse (cdr terms))))
    (cons* (car form) anno form)))

(include-from-path "mach.d/fpp-tab.scm")
(include-from-path "mach.d/fpp-act.scm")

(define read-comm (make-comm-reader '(("#" . "\n"))))
(define read-code-anno (make-comm-reader '(("@<" . "\n"))))
(define read-lone-anno (make-comm-reader '(("@" . "\n"))))
(define (mk-anno v s)
  (let ((x (or (string-index s (lambda (c) (not (char-whitespace? c)))) 0)))
    (cons v (substring s x))))

(define (swap pair) (cons (cdr pair) (car pair)))
(define (skip-ws ch) (if (char-whitespace? ch) (skip-ws (read-char)) ch))

(define-public make-fpp-lexer-generator
  (let* ((match-table fpp-mtab)
         (read-string read-c-string)
         (read-ident read-c$-ident)
         (space-cs (string->char-set " \t\r\f"))
         (strtab (filter-mt string? match-table))
         (kwstab (filter-mt like-c$-ident? strtab))
         (keytab (map-mt string->symbol kwstab))
         (chrseq (remove-mt like-c-ident? strtab))
         (symtab (filter-mt symbol? match-table))
         (read-chseq (make-chseq-reader chrseq))
         (newline-val (assoc-ref chrseq "\n"))
         (ident-val (assoc-ref match-table '$ident))
         (lone-anno-val (assoc-ref fpp-mtab '$lone-anno))
         (code-anno-val (assoc-ref fpp-mtab '$code-anno))
         (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair)))))
    (lambda ()
      (define (loop ch)
        (cond
         ((eof-object? ch) ;;(assc-$ (cons '$end ch))
          (if (pop-input) (loop (read-char)) (assc-$ (cons '$end ch))))
         ((eqv? ch #\newline) (cons newline-val "\n"))
         ((char-set-contains? space-cs ch) (loop (read-char)))
         ((read-comm ch #f) (loop (read-char)))
         ((read-code-anno ch) => (lambda (p) (mk-anno code-anno-val (cdr p))))
         ((read-lone-anno ch) => (lambda (p) (mk-anno lone-anno-val (cdr p))))
         ((read-ident ch) => (lambda (s)
                               (cond
                                ((assoc s kwstab) => swap)
                                ((char=? #\$ (string-ref s 0))
                                 (cons ident-val (substring s 1)))
                                (else (cons ident-val s)))))
         ((read-c-num ch) => assc-$)
         ((read-string ch) => assc-$)
         ((read-chseq ch))
         ((char=? ch #\\) (loop (skip-ws (read-char))))
         ((char=? ch #\return) (loop (read-char)))
         (else (cons ch (string ch)))))
      (if #t                  ; read properties
          (lambda ()
            (let* ((port (current-input-port))
                   (file (port-filename port))
                   (line (port-line port))
                   (props `((filename . ,file) (line . ,line) (column . 0)))
                   (lxm (loop (read-char))))
              (set-source-properties! lxm props)
              lxm))
          (lambda () (pk (loop (read-char))))))))

(define fpp-lexer (make-fpp-lexer-generator))

(define raw-parser
  (make-lalr-parser
   (acons 'act-v fpp-act-v fpp-tables)
   #:skip-if-unexp '("\n" $lone-anno $code-anno)))

(define* (parse-fpp #:key debug)
  (catch 'nyacc-error
    (lambda ()
      (raw-parser fpp-lexer #:debug debug))
    (lambda (key fmt . args)
      (apply simple-format (current-error-port) fmt args)
      (newline (current-error-port))
      #f)))

(define* (read-fpp-file filename #:key debug)
  (let* ((port (open-input-file filename))
         (tree (with-input-from-port port
                 (lambda() (parse-fpp #:debug debug)))))
    ; need to augment tree with filename ???
    tree))

;; --- last line ---
