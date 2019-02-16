#lang racket/base

;; utilities.rkt -- various utilities, copied and adapted from ActivityLog2
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

(require racket/contract)

(provide/contract

 (data-directory (-> path-string?))
 (preferences-file (-> path-string?))
 (put-pref (-> symbol? any/c any/c))
 (get-pref (-> symbol? any/c any/c))

 (set-dbglog-to-standard-output (-> boolean? any/c))
 (dbglog (->* (string?) () #:rest (listof any/c) any/c))
 (ignore-errors (->* ((-> any/c)) (#:name string?) any/c))
 (dbglog-exception (-> string? any/c any/c))
 (thread/dbglog (->* ((-> any/c))
                     (#:name string?
                      #:log-start boolean?
                      #:log-finish boolean?)
                     any/c))
 )

(require errortrace/errortrace-lib
         racket/async-channel
         racket/file
         racket/format
         racket/math
         racket/port
         racket/match
         racket/list

         (for-syntax
          racket/base
          racket/file
          racket/format
          racket/string
          racket/runtime-path))

;; Return the default place where data files are stored on this system.
(define (get-pref-dir)
  (if (eq? 'windows (system-type 'os))
      (let ([pref-dir (getenv "LOCALAPPDATA")])
        (if pref-dir
            (string->path pref-dir)
            (find-system-path 'pref-dir)))
      (find-system-path 'pref-dir)))

(define the-data-directory #f)

;; Return the default directory where the application will store its data
;; files.  This directory will be created if it does not exist.
(define (data-directory)
  (unless the-data-directory
    (let ((dir (get-pref-dir)))
      ;; dir might not exist, but make-directory* never fails
      (let ((pref-dir (build-path dir "ActivityLog")))
        (make-directory* pref-dir)
        (set! the-data-directory pref-dir))))
  the-data-directory)

(define the-preferences-file #f)

;; Return the name of the file used to store preferences.  Note that we use
;; the same preferences file as ActivityLog2, this is mainly so we can read
;; the database location from there.
(define (preferences-file)
  (unless the-preferences-file
    (set! the-preferences-file
          (build-path (data-directory) "ActivityLogPrefs.rktd")))
  the-preferences-file)

;; Store VALUE under NAME in the preferences file
(define (put-pref name value)
  (put-preferences (list name)
                   (list value)
                   (lambda (p) (error 'lock-fail "Failed to get the pref file lock" p))
                   (preferences-file)))

;; Retrieve the value for NAME from the preferences file, or return the value
;; of FAIL-THUNK if it does not exist.
(define (get-pref name fail-thunk)
  (get-preference name fail-thunk 'timestamp (preferences-file)))


(define the-log-port #f)                    ; port to which all log messages go
(define log-to-standard-output #f)          ; when #t dbglog also prints to stdout

(define (set-dbglog-to-standard-output flag)
  (set! log-to-standard-output flag)
  ;; Start counting lines on the current output port, so we know when to open
  ;; a fresh line in `dbglog`.
  (when log-to-standard-output
    (unless (port-counts-lines? (current-output-port))
      (port-count-lines! (current-output-port)))))

;; Open the log file if needed.  We use a single log file in append mode, we
;; don't expect the file to grow too much so we don't recyle it.  If it
;; becomes a problem, we can create a new file for each new invokation (or
;; some other strategy).
(define (maybe-init-the-log-port)
  (unless the-log-port
    (let ((fname (build-path (data-directory) "Al2-IRisk-Dashboard.log")))
      (set! the-log-port (open-output-file fname #:mode 'text #:exists 'append)))))

;; Return the current timestamp as a string.  Includes milliseconds.  It is
;; used to put timestamps in the log messages.
(define (get-current-timestamp)

  (define (fmt val width)
    (~a val #:width width #:align 'right #:pad-string "0"))

  (let ((ts (exact-round (current-inexact-milliseconds))))
    (let-values (([q r] (quotient/remainder ts 1000)))
      (let ((date (seconds->date q)))
        (string-append
         (fmt (date-year date) 4)
         "-"
         (fmt (date-month date) 2)
         "-"
         (fmt (date-day date) 2)
         " "
         (fmt (date-hour date) 2)
         ":"
         (fmt (date-minute date) 2)
         ":"
         (fmt (date-second date) 2)
         "."
         (fmt r 3))))))

;; Write MSG to the log file.  A timestamp is prepended and a newline is
;; appended.  The log port is flushed immediately, so it is not particularily
;; efficient to log many things...
(define (dbglog format-string . args)
  (define msg (apply format format-string args))
  (define ts (get-current-timestamp))
  (maybe-init-the-log-port)
  (define (do-log port)
    (write-string (get-current-timestamp) port)
    (write-string " " port)
    (write-string msg port)
    (write-string "\n" port)
    (flush-output port))
  (do-log the-log-port)
  (when log-to-standard-output
    (let ((out (current-output-port)))
      ;; Turn on line counting (if not already on) and write a new line before
      ;; the log message, if needed -- this ensures all log messages are on
      ;; lines of their own...  We also do this here, in case
      ;; `current-output-port` has changed since
      ;; `set-dbglog-to-standard-output` was called.
      (unless (port-counts-lines? out)
        (port-count-lines! out))
      (define-values (line column position) (port-next-location out))
      (when (and column (not (zero? column)))
        (write-string "\n" out))
      (do-log out))))

;; Log an exception, WHO is prepended to the log message, can be the function
;; name that calls `dbglog-exception'
(define (dbglog-exception who e)
  ;; NOTE: 'print-error-trace' will only print a stack trace if the error
  ;; trace library is used.  To use it, remove all .zo files and run "racket
  ;; -l errortrace -t run.rkt"
  (let ((message (if (exn? e) (exn-message e) e))
        (call-stack (if (exn? e)
                        (call-with-output-string
                         (lambda (o) (print-error-trace o e)))
                        "#<no call stack>")))
    (dbglog "~a: ~a ~a" who message call-stack)))

;; Run THUNK, catching all exceptions and logging them.
(define (ignore-errors thunk #:name (name "*unnamed*"))
  (with-handlers
    (((lambda (e) #t)
      (lambda (e) (dbglog (format "thunk <~a>: ~a" name e)))))
    (thunk)))

;; Wrapper around `thread', log a message if THUNK throws an exception and
;; optionally log messages when the thread starts and finishes.
(define (thread/dbglog thunk
                       #:name [name "*unnamed*"]
                       #:log-start [log-start #f]
                       #:log-finish [log-finish #f])
  (thread
   (lambda ()
     (with-handlers
       (((lambda (e) #t)
         (lambda (e) (dbglog (format "thread <~a>: ~a" name e)))))
       (when log-start (dbglog (format "thread <~a> started" name)))
       (thunk)
       (when log-finish (dbglog (format "thread <~a> completed" name)))))))
