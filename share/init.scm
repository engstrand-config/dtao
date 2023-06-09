(use-modules (ice-9 exceptions))

(define* (raise-error message)
  "Raises an exception with message MESSAGE."
  (raise-exception
   (make-exception-with-message
    (string-append "dtao: " message))))

(define* (dtao:start-repl-server)
  "Starts a local Guile REPL server, listening on a UNIX socket at path
@path{/tmp/dtao-guile.socket}. This REPL allows you to execute expressions
in the dtao-guile context with a user-friendly interface.

The preferred way of connecting to the REPL server is using Geiser in Emacs.
You can connect to the server by calling @code{geiser-connect-local} and
specifying the UNIX-socket path.

Note that this needs to be explicitly called in order for the REPL server to
be started!"
  (use-modules (system repl server))

  ;; REPL socket path is dependent on the type of build, i.e. stable or devel.
  ;; Therefore, this variable is set during the initial configuration load in C.
  (define (kill-server)
    (when (file-exists? dtao:%repl-socket-path)
      (delete-file dtao:%repl-socket-path)
      (stop-server-and-clients!)))

  (kill-server)
  (spawn-server (make-unix-domain-server-socket #:path dtao:%repl-socket-path)))

(define* (define-block id
           #:key
           (signal 0)
           (interval 0)
           (events? #f)
           (render #f)
           (click #f))
  "Defines a new block that can be rendered via your
renderer procedure. If a block with the same id already exists,
the block will be updated with the new properties."
  (when render
    (unless (eq? (car (procedure-minimum-arity render)) 1)
      (raise-error "render function expects one argument (monitor)")))

  (when click
    (unless (eq? (car (procedure-minimum-arity click)) 1)
      (raise-error "click procedure expects one argument (monitor).")))

  (set 'blocks
       `((id . ,(format #f "~a" id))
         (signal . ,signal)
         (interval . ,interval)
         (events? . ,events?)
         (render . ,render)
         (click . ,click))))
