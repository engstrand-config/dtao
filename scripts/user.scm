#!/gnu/store/18hp7flyb3yid3yp49i6qcdq0sbi5l1n-guile-3.0.2/bin/guile --no-auto-compile
!#
(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1))

(define (update-blocks status-info blocks)
  (fold (lambda (block acc)
          (let ((str (block status-info)))
            (if (string? str)
                (string-append acc " " str)
                acc)))
        ""
        blocks))

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

(define (%block-gibberish info)
  "BREAKING NEWS: Fredrik fuckade upp igen")

(define %user-blocks
  (list
    %block-time
    %block-battery))

(define write-port (open-output-pipe "dtao -a -ta l -sa c -fg FFFFFF -fn \"JetBrains Mono:style=bold:size=13\""))

(define (update-user-blocks blocks iteration)
  (update-blocks '() blocks))

(define (statusbar)
  (let loop ((iteration 0) (previous-user-output ""))
    (begin
      (let* ((user-output (update-user-blocks %user-blocks iteration)))
        (unless (eq? previous-user-output user-output)
          (display (string-append "^sw()" user-output "\n")
                   write-port))
        (sleep 1)
        (loop (+ iteration 1) user-output)))))

(statusbar)
(close-pipe write-port)
