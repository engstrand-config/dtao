#!/gnu/store/18hp7flyb3yid3yp49i6qcdq0sbi5l1n-guile-3.0.2/bin/guile --no-auto-compile
!#
(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1))

(define (%block-time info)
  (let* ((port (open-input-pipe "date \"+%a %d %b (v.%V) %T \""))
         (result (read-line port)))
    (close-pipe port)
    (string-append "^ca(0,notify-send \"Hello world\")" result "^ca()")))

(define (%block-battery info)
  (let* ((port (open-input-file "/sys/class/power_supply/BAT0/capacity"))
         (result (read-line port)))
    (close-port port)
    (string-append "^fg(#00FF00)"result "%")))

(define (%block-tags info)
  (let ((selected '(1)))
    (string-join
      (map
        (lambda (t)
          (format #f "~a ~a ~a"
                  (if (member t selected) "^bg(#005577AA)^fg(#FFFFFF)" "")
                  (number->string t)
                  (if (member t selected) "^bg()^fg()" "")))
        (iota 9 1))
      "")))

(define (%block-gibberish info)
  "BREAKING NEWS: Fredrik fuckade upp igen")

(define (%block-title info)
  (unless (null? info)
    (let* ((dpy (assoc-ref info "DP-1"))
           (title (assoc-ref dpy "title")))
      (when (string? title)
        (string-append title)))))

(define %user-blocks
  (list
    %block-time))

(define %compositor-blocks
  (list
    %block-title))

(define read-port (current-input-port))
(define write-port (open-output-pipe "dtao -a -ta l -sa c -fg FFFFFF -z -z -fn \"JetBrains Mono:style=bold:size=13\""))

(define (update-blocks status-info blocks)
  (fold (lambda (block acc)
          (let ((str (block status-info)))
            (if (string? str)
                (string-append acc " " str)
                acc)))
        ""
        blocks))

(define (update-user-blocks blocks iteration)
  (update-blocks '() blocks))

(define (update-compositor-blocks blocks)
  (let loop ((line (read-line)) (status-info '()))
    (if (char-ready? read-port)
        (loop (read-line)
              (let* ((line-data (string-split line #\space))
                     (dpy (car line-data))
                     (saved-info (or (assoc-ref status-info dpy) '())))
                (assoc-set!
                  status-info
                  dpy
                  (cons `(,(cadr line-data) . ,(caddr line-data))
                        saved-info))))
        (update-blocks status-info blocks))))

; How should we handle stdin?
; We can not update blocks at intervals and also update whenever
; stdin data is available (without using a more comlex solution).
(define (statusbar)
  (let loop ((iteration 0) (previous-base-output "") (previous-user-output ""))
    (begin
      (let* ((base-output
               (if (char-ready? read-port)
                   (update-compositor-blocks %compositor-blocks)
                   previous-base-output))
             (user-output (update-user-blocks %user-blocks iteration)))
        (unless (and (eq? previous-user-output user-output)
                     (eq? previous-base-output base-output))
          (display (string-append "^tw()" (%block-tags '())
                                  "^sw()" user-output "\n")
                   write-port))
        (sleep 1)
        (loop (+ iteration 1) base-output user-output)))))

(statusbar)
(close-pipe write-port)
