#!/gnu/store/18hp7flyb3yid3yp49i6qcdq0sbi5l1n-guile-3.0.2/bin/guile --no-auto-compile
!#

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 rw)
             (srfi srfi-1)
             (rnrs arithmetic bitwise))

(define %workspaces 9)
(define %monitor "eDP-1")

(define (update-blocks state blocks)
  (fold (lambda (block acc)
          (let ((str (block state)))
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
    (if selected-tag
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
                (iota %workspaces 1))
    "fuck no")))

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

(define write-port (open-output-pipe "dtao -a -ta l -fg FFFFFF -z -z -fn \"JetBrains Mono:style=bold:size=13\""))

(define (await-input port state)
  (let loop ((line (read-line port)) (current-state state))
    (if (and (string? line) (not (eof-object? line)))
        (let* ((line-data (string-split line #\space))
               (dpy (car line-data))
               (key (cadr line-data))
               (dpy-state (or (assoc-ref current-state dpy) '())))
          (assoc-set!
            current-state
            dpy
            (if (eq? key "tags")
                (append `(("active-tags" . ,(caddr line-data))
                          ("selected-tag" . ,(cadddr line-data)))
                        dpy-state)
                (cons `(,key . ,(caddr line-data))
                      dpy-state)))
          ; (new-state (acons dpy
          ;                   (acons
          ;                     key
          ;                      (if (eq? type "tags")
          ;                           (append `(("active-tags" . ,(caddr line-data))
          ;                                     ("selected-tag" . ,(cadddr line-data)))
          ;                                   dpy-state)
          ;                           (cons `(,type . ,(caddr line-data))
          ;                                 dpy-state))))
          ;                   current-state)))
          (if (char-ready? port)
              (loop (read-line port) current-state)
              current-state))
        current-state)))

(define (write-output str iteration)
  (display (string-append "^tw()" str "iter: " (number->string iteration) "\n") write-port))

(define (statusbar port)
  (let loop ((state '()) (iteration 1))
    (write-output (update-blocks state %compositor-blocks) iteration)
    (loop (await-input port state) (+ iteration 1))))

(statusbar (current-input-port))
