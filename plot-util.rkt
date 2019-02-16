#lang racket/base

;; plot-util.rkt -- plot utilities, copied from ActivityLog2
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

(require racket/contract
         racket/class
         racket/gui/base
         racket/draw
         racket/snip
         racket/match
         racket/math
         pict
         plot/no-gui
         pict/snip
         plot/utils
         plot
         embedded-gui
         "utilities.rkt")

(provide good-hover? pu-label make-hover-badge)

;; Resources for drawing overlays on the plots.  Defined in one place to
;; ensure consistency across all the plots.

(define hover-tag-background (make-object color% #xff #xf8 #xdc 0.95))
(define hover-tag-item-color (make-object color% #x2f #x4f #x4f))
(define hover-tag-label-color (make-object color% #x77 #x88 #x99))
(define hover-tag-title-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define hover-tag-item-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define hover-tag-label-font (send the-font-list find-or-create-font 10 'default 'normal 'normal))
(define hover-tag-title-face (cons hover-tag-item-color hover-tag-title-font))
(define hover-tag-item-face (cons hover-tag-item-color hover-tag-item-font))
(define hover-tag-label-face (cons hover-tag-label-color hover-tag-label-font))


;; Return #t when the X, Y and EVENT passed on to a plot mouse callback are
;; valid to display hover information.  They are valid when X and Y are not #f
;; (they are #f when they are inside the plot snip but not on the plot itself,
;; for example in the axes area).  The mouse event must also be a motion event
;; and the SNIP must be directly under the mouse with no other snips above it.
(define (good-hover? snip x y event)

  ;; Return the editor which owns SNIP
  (define (get-editor snip)
    (let ([admin (send snip get-admin)])
      (and admin (send admin get-editor))))

  ;; Find the snip which is at the EVENT location in EDITOR (this is the
  ;; topmost snip in the editor)
  (define (find-snip editor event)
    (let ((ex (box (send event get-x)))
          (ey (box (send event get-y))))
      (send editor global-to-local ex ey)
      (define snip (send editor find-snip (unbox ex) (unbox ey)))
      snip))

  ;; Return true if SNIP is the same snip as the one under the location of
  ;; EVENT inside the editor.
  (define (same-snip snip event)
    (define other
      (let ([editor (get-editor snip)])
        (and editor (find-snip editor event))))
    ;; NOTE: the equal? call will raise an exception when comparing a plot
    ;; snip to a pict snip.  This is because the contract for the equal<%>
    ;; interface for image-snip% expects another image-snip% as an
    ;; argument. See also: https://github.com/racket/gui/issues/119
    (with-handlers
      (((lambda (e) #t) (lambda (e) #f)))
      (equal? snip other)))

  (and (real? x) (real? y)
       (is-a? event mouse-event%)
       (eq? (send event get-event-type) 'motion)
       #;(same-snip snip event)
       ))

;; Return a pict object representing a badge for displaying information on a
;; plot.  The ITEMS is a list of key-value string pairs and these are arranged
;; in a table format.
;;
;; As a special case, key can be #f, in which case only the value is rendered
;; using the "key" font and color.  This can be used to display additional
;; information about a value which will be shown underneath the value.
;;
;; NOTE: the returned pict object can be placed on a plot using
;; `add-pict-overlay`.
(define (make-hover-badge items)
  (define column-count
    (for/fold ((column-count 0)) ((item (in-list items)))
      (max column-count (length item))))
  (define picts '())
  (for ((item (in-list items)))
    (let* ((key (car item))
           (vals (reverse (cdr item)))
           (face (if key hover-tag-item-face hover-tag-label-face)))
      (for ((dummy (in-range (- column-count (add1 (length vals))))))
        (set! picts (cons (text "" face) picts)))
      (for ((val (in-list vals)))
        (set! picts (cons (text val face) picts)))
      (set! picts (cons (text (or key "") hover-tag-label-face) picts))))
  (let ((p0 (table column-count picts lc-superimpose cc-superimpose 15 3)))
    (cc-superimpose
     (filled-rounded-rectangle (+ (pict-width p0) 20) (+ (pict-height p0) 20) -0.1
                               #:draw-border? #f
                               #:color hover-tag-background)
     p0)))

;; Create a renderer that draws label, which can be either a string, a pict or
;; a list of them, to be used as an overlay.  The label is drawn at position
;; X, Y in plot coordinates.
;;
;; NOTES: any #f values in LABELS are discarded (this makes the use of
;; `pu-label` more convenient).  If multiple labels are provided they are
;; stacked vertically using `vl-append`.
(define (pu-label x y . labels)
  (if (and (= (length labels) 1) (pict? (car labels)))
      ;; Special case: a single pict passed in is displayed as is...
      (point-pict (vector x y) (car labels) #:point-sym 'none #:anchor 'auto)
      ;; Otherwise create new picts and pack them into a final pict
      (let* ((p0 (for/list ((label (in-list labels)) #:when label)
                   (if (pict? label)
                       label
                       (text label hover-tag-item-font))))
             (p1 (if (= (length p0) 1)
                     (car p0)
                     (apply vl-append 3 p0)))
             (p2 (cc-superimpose
                  (filled-rounded-rectangle (+ (pict-width p1) 10)
                                            (+ (pict-height p1) 10) -0.1
                                            #:draw-border? #f
                                            #:color hover-tag-background)
                  p1)))
        (point-pict (vector x y) p2 #:point-sym 'none #:anchor 'auto))))
