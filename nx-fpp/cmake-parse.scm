;; cmake-parse.scm

;; Copyright (C) 2025 Matthew Wette
;; SPDX-License-Identifier: LGPL-3.0-or-later

;;
;; https://cmake-format.readthedocs.io/en/latest/parse-tree.html

(define (sf fmt . args) (apply simple-format (current-error-port) fmt args))
(use-modules (ice-9 pretty-print))
(define (pp exp) (pretty-print exp (current-error-port) #:per-line-prefix "  "))

;; 'l-par  : LEFT_PAREN
;; 'r-par  : RIGHT_PAREN
;; 'nl     : NEWLINE
;; 'ws     : whitespace
;; 'numb   : NUMBER
;; 'word   : WORD
;; 'q-lit  : QUOTED_LITERAL
;; 'u-lit  : UNQUOTED_LITERAL
;; 'comm   : COMMENT
;; 'deref  : DEREF  ${xxx}

;; 'br-com : BRACKET_COMMENT
;; 'f-OFF  : FORMAT_OFF cmake_format:OFF
;; 'f-ON   : FORMAT_OFF cmake_format:ON
;; 'br-arg : BRACKET_ARG [=[xxxx]=]

;; adding 'eof

(define word-0-cs
  (char-set-union
   (string->char-set "abcdefghijklmnopqrstuvwxyz")
   (string->char-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
   (string->char-set "_")))
(define word-1-cs
  (char-set-union word-0-cs (string->char-set "0123456789")))
(define number-cs
  (string->char-set "0123456789."))

(define (next-tok)
  (define rls reverse-list->string)
  (let loop ((chl '()) (st 0) (ch (read-char)))
    (case st
      ((0)
       (cond
        ((eof-object? ch) (cons 'eof ch))
        ((char=? #\( ch) (cons 'l-par "("))
        ((char=? #\) ch) (cons 'r-par ")"))
        ((char=? #\newline ch) (cons 'nl "\n"))
        ((char=? #\# ch) (loop '() 10 (read-char))) ; comm
        ((char=? #\" ch) (loop '() 20 (read-char))) ; q-lit
        ((char-set-contains? word-0-cs ch) (loop chl 40 ch))  ; word
        ((char-set-contains? number-cs ch)  (loop chl 50 ch)) ; number
        ((char-whitespace? ch) (loop chl 60 ch))              ; ws
        ((char=? #\$ ch) (loop chl 70 (read-char)))           ; deref
        (else (loop chl 80 ch))))                             ; u-lit
      ((10) ;; comment
       (loop chl (if (char=? #\newline ch) 0 st) (read-char)))
      ((20) ;; string
       (cond
        ((char=? #\\ ch) (loop (cons ch chl) 21 (read-char)))
        ((char=? #\" ch) (cons 'q-lit (rls chl)))
        (else (loop (cons ch chl) st (read-char)))))
      ((21) ;; escaped char
       (loop (cons ch chl) 20 (read-char)))
      ((40) ;; WORD
       (cond
        ((char-set-contains? word-1-cs ch) (loop (cons ch chl) st (read-char)))
        (else (unread-char ch) (cons 'word (rls chl)))))
      ((50) ;; 'numb
       (cond
        ((char-set-contains? number-cs ch)  (loop (cons ch chl) st (read-char)))
        (else (unread-char ch) (cons 'numb (rls chl)))))
      ((60) ;; 'ws
       (cond
        ((char-whitespace? ch) (loop (cons ch chl) st (read-char)))
        (else (unread-char ch) (cons 'ws (rls chl)))))
      ((70) ;; 'deref
       (cond
        ((char=? #\{ ch) (loop chl 71 (read-char)))
        (else (loop (list #\$) 80 ch))))
      ((71)
       (cond
        ((char=? #\} ch) (cons 'u-lit (rls chl)))
        (else (loop (cons ch chl) st (read-char)))))
      ((80) ;; 'u-lit
       (cond
        ((char-whitespace? ch) (unread-char ch) (cons 'u-lit (rls chl)))
        ((char=? #\newline ch) (unread-char ch) (cons 'u-lit (rls chl)))
        (else (loop (cons ch chl) st (read-char)))))
      )))

(define tok-util
  (let ((pb-tok #f))                      ; pushback token
    (case-lambda
      ((tok)
       (set! pb-tok tok))
      (()
       (let ((tok pb-tok))
         (cond
          (tok (set! pb-tok #f) tok)
          (else (next-tok))))))))

(define get-tok tok-util)
(define unget-tok tok-util)

;; parse
;; 'body    : BODY
;; 'ws      : WHITESPACE
;; 'comm    : COMMENT
;; 'stmt    : STATEMENT
;; 'flow    : FLOW_CONTROL (if foreach)
;; 'argg    ; ARGGROUP
;; 'pargg   : PARGGROUP (positional args)
;; 'flagg   : FLAGGROUP
;; 'kwargg  : KWARGROUP
;; 'pareng  : PARENGROUP
;; 'fname   : FUNNAME
;; 'arg     : ARGUMENT
;; 'kw      : KEYWORD
;; 'fl      : FLAG
;; 'oosw    : ONOFFSWITCH

(define (tok-is? tok type)
  (and (pair? tok) (eq? (car tok) type) tok))

(define (p-body tok)
  (let loop ((kids '()) (tok tok))
    (cond
     ((not tok) `(body . ,(reverse kids)))
     ((tok-is? tok 'comm) (loop (cons tok kids) (get-tok)))
     ((tok-is? tok 'ws) (loop (cons tok kids) (get-tok)))
     ((p-stmt tok) =>
      (lambda (res)
        (if res
            (loop (cons res kids) (get-tok))
            (error "p-body parse error")))))))

(define (p-arglist tok)
  (let loop ((args '()) (tok tok))
    (cond
     ((tok-is? tok 'comm) (loop args (get-tok)))
     ((tok-is? tok 'nl) (loop args (get-tok)))
     ((tok-is? tok 'ws) (loop args (get-tok)))
     ((tok-is? tok 'word) => (lambda (t) (loop (cons t args) (get-tok))))
     ((tok-is? tok 'numb) => (lambda (t) (loop (cons t args) (get-tok))))
     ((tok-is? tok 'q-lit) => (lambda (t) (loop (cons t args) (get-tok))))
     ((tok-is? tok 'u-lit) => (lambda (t) (loop (cons t args) (get-tok))))
     ((tok-is? tok 'r-par) `(arglist ,@(reverse args)))
     (else (sf "p-arglist error:\n") (pp tok) (quit)))))

(define (p-stmt tok)
  (and
   (tok-is? tok 'word)
   (let ((tok1 (get-tok)))
     (if (tok-is? tok1 'l-par)
         (and=> (p-arglist (get-tok))
           (lambda (t) `(stmt ,t)))))))
 
(let ((file "CMakeLists.txt")
      )
  (with-input-from-file file
    (lambda ()
      (let loop ((pts '()) (tok (get-tok)))
        (cond
         ((tok-is? tok 'comm) (loop pts (get-tok)))
         ((tok-is? tok 'nl) (loop pts (get-tok)))
         ((tok-is? tok 'eof) `(body ,@(reverse pts)))
         ((p-stmt tok) => (lambda (pt) (loop (cons pt pts) (get-tok))))
         (else (error "parse error")))))))

;; --- last line ---
