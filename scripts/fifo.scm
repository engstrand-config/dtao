#!/gnu/store/18hp7flyb3yid3yp49i6qcdq0sbi5l1n-guile-3.0.2/bin/guile --no-auto-compile
!#

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 rw)
             (srfi srfi-1)
             (rnrs arithmetic bitwise))

; (define fd (open-fdes "/tmp/dwl-guile.fifo" O_RDONLY))
; (define fd-port (fdes->inport fd))

; (define (loop)
;   (let ((fds (select (list fd) '() '())))
;     ; (display (read-string!/partial (string) fd-port))
;     (display (read-line fd-port))
;     (loop)))

(define port (open "/tmp/dwl-guile.fifo" O_RDONLY))

(define (loop)
  (display (read-line port))
  (usleep 100)
  (loop))

(loop)
