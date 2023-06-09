(use-modules (ice-9 exceptions))

(define* (raise-error message)
  "Raises an exception with message MESSAGE."
  (raise-exception
   (make-exception-with-message
    (string-append "dtao: " message))))

(define* (dtao:block-group
          #:key
          (flex 1)
          (delimiter #f)
          (align 'left)
          (bg-color "#000000")
          (fg-color "#FFFFFF")
          (content '()))
  "Creates a block group inside a layout.
The group will contain CONTENT, which can be either a list of
block names, or a function. Content inside the group will be separated
by DELIMITER and aligned inside the group according to ALIGN. If FLEX is
set to true, the group will expand as much as possible. Increasing FLEX
to values larger than other block groups will result in it expanding more
compared to others. BG-COLOR and FG-COLOR are used to set the default
colors for content inside the group, including the group area itself."
  (unless (or (list? content) (procedure? content))
    (raise-error "expected content to be a list or function"))

  (when (procedure? content)
      (unless (eq? (car (procedure-minimum-arity content) 1))
        (raise-error "content function expects one argument (monitor)")))

  `((flex . ,flex)
    ,@(if (eq? delimiter #f)
          '()
          `((delimiter . ,delimiter)))
    (align . ,align)
    (bg-color . ,bg-color)
    (fg-color . ,fg-color)
    (content . ,content)))

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

(define* (dtao:make-block id
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
      (raise-error "click procedure expects two arguments (monitor, button).")))

  (set 'blocks
       `((id . ,(format #f "~a" id))
         (signal . ,signal)
         (interval . ,interval)
         (events? . ,events?)
         (render . ,render)
         (click . ,click))))

(define* (dtao:make-layout id layout)
  "Defines a layout with id ID. LAYOUT should be a list of
block groups and/or block ids. If multiple layouts are used,
you can select which layout to render using @code{dtao:set-renderer}.
If no renderer is set, the first created layout will be used."
  (unless (list? layout)
    (raise-error "layout should be a list"))
  (set 'layouts
       `((id . ,(format #f "~a" id))
         (layout . ,layout))))

(define* (dtao:set-renderer func)
  "Configures a custom renderer function used to
select which layout to render for the current state."
  (unless (eq? (car (procedure-minimum-arity func)) 2)
    (raise-error "renderer function expectes two arguments \
(monitor, current-state)"))
  (set 'renderer func))
