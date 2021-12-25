#!/gnu/store/18hp7flyb3yid3yp49i6qcdq0sbi5l1n-guile-3.0.2/bin/guile --no-auto-compile
!#

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1)
             (rnrs arithmetic bitwise))

(define %workspaces 9)
(define %monitor "DP-1")

(define (update-blocks status-info blocks)
  (fold (lambda (block acc)
          (let ((str (block status-info)))
            (if (string? str)
                (if (eq? (string-length acc) 0)
                    str
                    (string-append acc " " str))
                acc)))
        ""
        blocks))

;; TODO: Respect dwl-guile workspace names
(define (%block-tags info)
  (let* ((dpy (assoc-ref info %monitor))
         (selected-tag (assoc-ref dpy "selected-tag")))
    (fold-right (lambda (tag acc)
                  (let ((selected (and (number? selected-tag)
                                       (logbit? (- tag 1) selected-tag))))
                    (string-append
                      (format #f "~a ~a ~a"
                              (if selected "^bg(#FF0000)^fg(#FFFFFF)" "")
                              (number->string tag)
                              (if selected "^bg()^fg()" ""))
                      acc)))
                ""
                (iota %workspaces 1))))

(define (%block-title info)
  (unless (null? info)
    (let* ((dpy (assoc-ref info %monitor))
           (title (assoc-ref dpy "title")))
      (when (string? title)
        (string-append title)))))

(define %compositor-blocks
  (list
    %block-tags
    %block-title))

(define read-port (current-input-port))
(define write-port (open-output-pipe "dtao -a -ta l -fg FFFFFF -z -z -fn \"JetBrains Mono:style=bold:size=13\""))

(define (update-compositor-blocks blocks status-info)
  (let loop ((line (read-line)))
    (if (eof-object? line)
        "hello world"
        (begin
          ;; Update state
          (let* ((line-data (string-split line #\space))
                 (dpy (car line-data))
                 (saved-info (or (assoc-ref status-info dpy) '())))
            (assoc-set!
              status-info
              dpy
              (let ((type (cadr line-data)))
                (if (eq? type "tags")
                    (append `(("active-tags" . ,(caddr line-data))
                              ("selected-tag" . ,(cadddr line-data)))
                            saved-info)
                    (cons `(,type . ,(caddr line-data))
                          saved-info)))))
          ;; Check if there is more data to be read
          (if (char-ready? read-port)
              (loop (read-line))
              (update-blocks status-info blocks))))))

(define (statusbar)
  (let loop ((previous-base-output (%block-tags '())) (current-state '()) (iteration 0))
    (begin
      (display (string-append "^tw()" previous-base-output (number->string iteration) "\n") write-port)
      (loop (update-compositor-blocks %compositor-blocks current-state) current-state (+ iteration 1)))))

(statusbar)
(close-pipe write-port)
