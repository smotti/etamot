#lang racket/gui

(require libnotify
         racket/snip
         threading)

(define etamot-frame%
  (class frame%
    (define/override (on-subwindow-char w e)
      (define key-code (send e get-key-code))
      (when (or (send e get-meta-down)
                (eqv? key-code #\space)
                (eqv? key-code #\backspace))
        (cond
          [(eqv? key-code #\p) (send pomodoro-button
                                     command (new control-event% [event-type 'button]))]
          [(eqv? key-code #\s) (send short-break-button
                                     command (new control-event% [event-type 'button]))]
          [(eqv? key-code #\l) (send long-break-button
                                     command (new control-event% [event-type 'button]))]
          [(eqv? key-code #\r) (send reset-time-button
                                     command (new control-event% [event-type 'button]))]
          [(eqv? key-code #\space) (send start-time-button
                                         command (new control-event% [event-type 'button]))]
          [(eqv? key-code #\backspace) (send stop-time-button
                                             command (new control-event% [event-type 'button]))])))
    (super-new)))

(define frame (new etamot-frame%
                   [label "Etamot"]))
(send frame focus)

;;;
;;; Utilities
;;;

(define (parse-time t)
  "Parses the time t specified in seconds to format mm:ss"
  (define (minutes-and-seconds) (quotient/remainder t 60))
  (define (format-time m s)
    (apply format "~a:~a"
           (map (lambda (v)
                  (~v v
                      #:left-pad-string "0" #:width 2 #:limit-marker ""
                      #:align 'right))
                (list m s))))
  (call-with-values
   minutes-and-seconds
   format-time))

;;;
;;; Pomodoro & Break Timer
;;;

(struct mode (name time message))

;; all times in seconds
(define *pomodoro* (mode 'pomodoro (* 25 60) "Time to take a break."))
(define *short-break* (mode 'short-break (* 5 60) "Time to do some work."))
(define *long-break* (mode 'long-break (* 25 60) "Get back to work lazy."))

(define *current-mode* *pomodoro*)
(define *current-time* (mode-time *pomodoro*))

(define (set-current-mode! m)
  (set! *current-mode* m))

(define (set-current-time! t)
  (set! *current-time* t))

;;;
;;; Menu Bar
;;;

(define menu-bar
  (new menu-bar%
       [parent frame]))

(define file-menu
  (new menu%
       [parent menu-bar]
       [label "File"]))

(define exit-menu-item
  (new menu-item%
       [parent file-menu]
       [label "Exit"]
       [callback (lambda (m e) (exit))]))

(define ?-menu
  (new menu%
       [parent menu-bar]
       [label "?"]))

(define about-dialog
  (new dialog%
       [label "About"]
       [style '(close-button)]))

(define about-message
  (new message%
       [parent about-dialog]
       [label "Etamot Version 0.1"]))

(define about-menu-item
  (new menu-item%
       [parent ?-menu]
       [label "About"]
       [callback (lambda (m e) (send about-dialog show #t))]))

;;;
;;; Top-Level Frame Layout
;;;


(define process-buttons-panel
  (new horizontal-panel%
       [parent frame]
       [alignment '(center center)]))

(define time-display-panel
  (new vertical-pane%
       [parent frame]
       [alignment '(center center)]))

(define time-buttons-panel
  (new horizontal-panel%
       [parent frame]
       [alignment '(center center)]))

;;;
;;; Process Buttons
;;;
(define pomodoro-button
  (new button%
       [parent process-buttons-panel]
       [label "Pomodoro"]
       [callback (lambda (b e)
                   (define pomodoro-time (mode-time *pomodoro*))
                   (set-current-mode! *pomodoro*)
                   (set-current-time! pomodoro-time)
                   (send time-display set-label (parse-time pomodoro-time)))]))

(define short-break-button
  (new button%
       [parent process-buttons-panel]
       [label "Short Break"]
       [callback (lambda (b e)
                   (define short-break-time (mode-time *short-break*))
                   (set-current-mode! *short-break*)
                   (set-current-time! short-break-time)
                   (send time-display set-label (parse-time short-break-time)))]))

(define long-break-button
  (new button%
       [parent process-buttons-panel]
       [label "Long Break"]
       [callback (lambda (b e)
                   (define long-break-time (mode-time *long-break*))
                   (set-current-mode! *long-break*)
                   (set-current-time! long-break-time)
                   (send time-display set-label (parse-time long-break-time)))]))

;;;
;;; Time Display
;;;

(define time-display
  (new message%
       [parent time-display-panel]
       [label (parse-time *current-time*)]
       [font (make-object font% 24 'default)]))

;;;
;;; Time Buttons
;;;

; Not pretty but don't know how to send a message to an instance that's being
; created.
(define timer #f)
(set! timer
  (new timer%
       [notify-callback (lambda ()
                          (set-current-time! (- *current-time* 1))
                          (send time-display
                                set-label (parse-time *current-time*))
                          (when (eqv? *current-time* 0)
                            (~> (new notification%
                                     [summary "Etamot"]
                                     [body (mode-message *current-mode*)])
                                (send show))
                            (send timer stop)))]))

(define start-time-button
  (new button%
       [parent time-buttons-panel]
       [label "Start"]
       [callback (lambda (b e)
                   (send timer start 1000))]))

(define stop-time-button
  (new button%
       [parent time-buttons-panel]
       [label "Stop"]
       [callback (lambda (b e)
                   (send timer stop))]))

(define reset-time-button
  (new button%
       [parent time-buttons-panel]
       [label "Reset"]
       [callback (lambda (b e)
                   (send timer stop)
                   (send time-display
                         set-label (parse-time (mode-time *current-mode*))))]))

;;;
;;; Show the Window
;;;

(send frame show #t)
