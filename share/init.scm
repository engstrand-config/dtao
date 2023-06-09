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
