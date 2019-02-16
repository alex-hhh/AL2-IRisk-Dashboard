#lang racket/base

;; dbutil.rkt -- database utilities, copied from ActivityLog2
;;
;; This file is part of AL2-IRisk-Daskboard
;; https://github.com/alex-hhh/AL2-IRisk-Dashboard
;; Copyright (c) 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(require db)
(provide define-sql-statement)

;; Read the next SQL statement from INPUT-PORT.  The statement is assumed to
;; be terminated by the #\; character.
(define (read-next-statement input-port)
  (let ((out (open-output-string))
        (in-string? #f))

    ;; Return the next character in the input stream PORT, collapsing all
    ;; whitespace to a single space and skipping all comments.  Comments start
    ;; with "--" and extend until the end of the line.  Strings are being
    ;; tracked for.
    (define (get-next-char)
      (let ((ch (read-char input-port)))

        (when (eqv? ch #\')
          (set! in-string? (not in-string?)))

        (cond ((eqv? ch eof) ch)

              ((and (char-whitespace? ch)
                    (let ((ch-next (peek-char input-port)))
                      (or (eqv? ch-next eof)
                          (char-whitespace? ch-next))))
               ;; Colapse all whitespace into one single space
               (get-next-char))

              ((and (not in-string?)
                    (eqv? ch #\-)
                    (eqv? (peek-char input-port) #\-))
               ;; This is a comment, skip it until end of line
               (for ((v (in-producer (lambda () (read-char input-port)) #\newline)))
                 (begin #f))
               #\ )

              ((char-whitespace? ch) #\ ) ; all whitespace converted to space
              (#t ch))))

    ;; read from the input stream using GET-NEXT-CHAR until a semi-colon (#\;)
    ;; is seen.  Intermediate read chars are written to OUT.  The full
    ;; statement is returned, or #f on EOF.
    (define (loop)
      (let ((ch (get-next-char)))
        (cond ((eqv? ch eof) ; incomplete statement
               #f)
              ((and (eqv? ch #\;) (not in-string?))
               (get-output-string out))
              (#t
               (write-char ch out)
               (loop)))))

    (loop)))


;; Define a `virtual-statement` containing an SQL statement read from a file.
;; This macro is used as (define-sql-statement name file) and defines name to
;; be a function of no arguments which returns a `virtual-statement` when
;; invoked.  The SQL statement will be read from the specified file the first
;; time the function is called.
;;
;; This is intended to be used instead of defining SQL statements in the
;; Racket source code as strings.  They can be defined in separate files where
;; they can be nicely formatted, commented and tested in the sqlite command
;; prompt.

(define (define-sql-statement path)
  (let ([vq #f])
    (lambda ()
      (unless vq
        (let ([s (call-with-input-file path read-next-statement)])
          (set! vq (virtual-statement (lambda (_) s)))))
      vq)))
