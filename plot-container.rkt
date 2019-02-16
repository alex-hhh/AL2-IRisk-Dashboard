#lang racket/gui

;; plot-container.rkt -- a canvas% capable of holding multiple plots.
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

(require pict)

(provide plot-container%)

(define (draw-centered-message dc msg color font)
  (let-values (([cw ch] (send dc get-size))
               ([w h x y] (send dc get-text-extent msg font #t)))
    (send dc set-font font)
    (send dc set-text-foreground color)
    (let ((ox (- (/ cw 2) (/ w 2)))
          (oy (- (/ ch 2) (/ h 2))))
      (send dc draw-text msg ox oy))))

(define message-font
  (send the-font-list find-or-create-font 36 'default 'normal 'normal))
(define message-color "gray")

(define read-only-pasteboard%
  (class pasteboard%
    (init-field [writable? #f])

    ;; Message to be shown when there is no main snip in the canvas.
    (define background-message #f)

    (define/public (set-writable w?) (set! writable? w?))

    ;; (define/augment (can-change-style? start len) writable?)
    (define/augment (can-delete? snip) writable?)
    (define/augment (can-insert? snip before x y) writable?)
    (define/augment (can-load-file? filename format) writable?)
    (define/augment (can-save-file? filename format) writable?)
    (define/augment (can-move-to? snip x y dragging?) writable?)
    (define/augment (can-select? snip on?) writable?)

    (define/override (can-do-edit-operation? op [recursive? #t])
      (case op
        [(copy select-all) #t]
        [else    writable?]))

    (define/public (set-background-message msg)
      (set! background-message msg))

    (define hover-pict #f)
    (define hover-pict-x #f)
    (define hover-pict-y #f)

    (define/public (set-hover-pict pict x y)
      (set! hover-pict pict)
      (set! hover-pict-x x)
      (set! hover-pict-y y))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (if before?
          ;; Draw a message when there is no snip in the pasteboard.
          (unless (send this find-first-snip)
            (send dc clear)
            (when background-message
              (draw-centered-message dc background-message "gray" message-font)))
          (when hover-pict
            (let ((o 10))
              (define-values (cw ch) (send dc get-size))
              (let ((x (if (< (+ hover-pict-x o (pict-width hover-pict)) cw)
                           (+ hover-pict-x o)
                           (- hover-pict-x o (pict-width hover-pict))))
                    (y (if (< (+ hover-pict-y o (pict-height hover-pict)) ch)
                           (+ hover-pict-y o)
                           (- hover-pict-y o (pict-height hover-pict)))))
                (draw-pict hover-pict dc x y))))))

    (super-new)
    (send this set-selection-visible #f)
    (send this set-area-selectable #f)

    ))

(define (with-writable pb thunk)
  (dynamic-wind
    (lambda () (send pb set-writable #t))
    thunk
    (lambda () (send pb set-writable #f))))

(define (with-edit-sequence pb thunk)
  (dynamic-wind
    (lambda () (send pb begin-edit-sequence))
    thunk
    (lambda () (send pb end-edit-sequence))))

(define (layout-info pb snip-count columns (spacing 5))
  (define c (send pb get-canvas))
  (define rows (exact-ceiling (if (> columns 0) (/ snip-count columns) 0)))
  
  (define-values (w h) (send c get-client-size))
  (define hinset (send c horizontal-inset))
  (define vinset (send c vertical-inset))
  (define hmargin (send c horiz-margin))
  (define vmargin (send c vert-margin))

  (define width (- w hinset hinset hmargin hmargin))
  (define height (- h vinset vinset vmargin vmargin))
  (define cell-width
    (if (> columns 0)
        (exact-floor (/ (- width (* (sub1 columns) spacing)) columns))
        0))
  (define cell-height
    (if (> rows 0)
        (exact-floor (/ (- height (* (sub1 rows) spacing)) rows))
        0))

  (values (+ hinset hmargin) (+ vinset vmargin) columns rows cell-width cell-height))

(define (snip-count pb)
  (let loop ([snip (send pb find-first-snip)] [count 0])
    (if snip
        (loop (send snip next) (add1 count))
        count)))

(define (layout-snips pb columns (spacing 5))
  (define-values (x0 y0 c rows cwidth cheight)
    (layout-info pb (snip-count pb) columns spacing))
  
  (when (and (> cwidth 0) (> cheight 0))
    (with-writable pb
      (lambda ()
        (with-edit-sequence pb
          (lambda ()
            (let loop ([snip (send pb find-first-snip)] [index 0])
              (when snip
                (let-values (((row col) (quotient/remainder index columns)))
                  (let ((x (+ x0 (* col (+ cwidth spacing))))
                        (y (+ y0 (* row (+ cheight spacing)))))
                    (send pb move-to snip x y)))
                (send pb resize snip cwidth cheight)
                (loop (send snip next) (add1 index))))))))))

(define plot-container%
  (class editor-canvas%
    (init parent
          [style null]
          [label #f]
          [horizontal-inset 5]
          [vertical-inset 5]
          [enabled #t]
          [vert-margin 0]
          [horiz-margin 0]
          [min-width 0]
          [min-height 0]
          [stretchable-width #t]
          [stretchable-height #t])
    (init-field [columns 3] [spacing 5])

    (define pb (new read-only-pasteboard% [writable? #f]))

    (super-new [parent parent]
               [editor pb]
               [horizontal-inset horizontal-inset]
               [vertical-inset vertical-inset]
               [label label]
               [enabled enabled]
               [style (list* 'no-hscroll 'no-vscroll style)]
               [vert-margin vert-margin]
               [horiz-margin horiz-margin]
               [min-width min-width]
               [min-height min-height]
               [stretchable-width stretchable-width]
               [stretchable-height stretchable-height])

    (define/override (on-size w h)
      (super on-size w h)
      (queue-callback (lambda () (layout-snips pb columns spacing))))

    (define/public (cell-dimensions snip-count)
      (define-values (x0 y0 c r cwidth cheight)
        (layout-info pb snip-count columns spacing))
      (values cwidth cheight))

    (define/public (add-snip snip)
      (with-writable pb (lambda () (send pb insert snip)))
      (queue-callback (lambda () (layout-snips pb columns))))

    (define/public (add-multiple-snips snips)
      (with-writable pb (lambda () (for ([snip (in-list snips)]) (send pb insert snip))))
      (queue-callback (lambda () (layout-snips pb columns))))

    (define/public (clear)
      (with-writable pb
        (lambda ()
          (send pb select-all)
          (send pb clear))))

    (define/public (set-background-message msg)
      (send pb set-background-message msg)
      (send this refresh))

    (define/public (set-hover-pict pict x y)
      (send pb set-hover-pict pict x y)
      (send this refresh))

    ))
