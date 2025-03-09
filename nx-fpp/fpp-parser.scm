;; fpp/parser.scm

;; Copyright (C) 2025 Matthew Wette
;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (fpp parser)
  #:export (parse-fpp read-fpp-file)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  #:use-module (sxml fold)
  #:use-module ((srfi srfi-1) #:select (fold)))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

(include-from-path "fpp/mach.d/fpp-tab.scm")
(include-from-path "fpp/mach.d/fpp-act.scm")

(define read-comm (make-comm-reader '(("#" . "\n")) #:eat-newline #f))
(define read-code-anno (make-comm-reader '(("@<" . "\n")) #:eat-newline #f))
(define read-lone-anno (make-comm-reader '(("@" . "\n")) #:eat-newline #f))

(define (swap pair) (cons (cdr pair) (car pair)))

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
         (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair)))))
    (lambda ()
      (define (loop ch)
        (cond
         ((eof-object? ch) (assc-$ (cons '$end ch)))
         ((char=? ch #\\) (read-char) (loop (read-char))) ;; kludgy
         ((eqv? ch #\newline) (cons newline-val "\n"))
         ((char-set-contains? space-cs ch) (loop (read-char)))
         ((read-comm ch #f) (loop (read-char)))
         ((read-code-anno ch #f) => assc-$)
         ((read-lone-anno ch #f) => assc-$)
         ((read-ident ch) => (lambda (s)
                               (or (and=> (assoc s kwstab) swap)
                                   (assc-$ (cons '$ident s)))))
         ((read-c-num ch) => assc-$)
         ((read-string ch) => assc-$)
         ((read-chseq ch))
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

(define (read-fpp-file filename)
  (let* ((port (open-input-file filename))
         (tree (with-input-from-port port parse-fpp))
         )
    ; need to augment tree with filename ???
    tree))

;; --- last line ---
