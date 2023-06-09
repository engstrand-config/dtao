;; General settings
(setq background-color "#000000"
      text-color "#FFFFFF"
      position 'top
      font "..."
      height 25
      layer 'LAYER-OVERLAY
      padding-left 0
      padding-right 0
      padding-top 0
      padding-bottom 0)

(dtao:make-block 'tags
  #:events? #t
  #:render
  (lambda (monitor)
    <render>))

(dtao:make-block 'layout
  #:events? #t
  #:render
  (lambda (monitor)
    (dtao:monitor-layout monitor)))

(dtao:make-block 'title
  #:events? #t
  #:render
  (lambda (monitor)
    (dtao:monitor-title monitor)))

(dtao:make-block 'battery
  #:interval 30
  #:render
  (lambda (_)
    <render>))

(dtao:make-block 'time
  #:interval 1
  #:render
  (lambda (_)
    <render>))

(dtao:make-block 'volume
  #:signal 10 ;; Re-render on signal
  #:render
  (lambda (_)
    <render>))

(dtao:make-block 'error-message
  #:events? #t
  #:click (lambda (_) <action>)
  #:render
  (lambda (_)
    (dtao:get-dwl-error-message)))

(dtao:make-layout
 'default-layout
 (list
  (dtao:block-group
   #:content '(tags layout))
  (dtao:block-group
   #:align 'center
   #:content '(title))
  (dtao:block-group
   #:delimiter " "
   #:align 'right
   #:content
   (lambda (monitor)
     (if (dtao:monitor-focused monitor)
         '(battery time)
         '())))))

(dtao:make-layout
 'error-layout
 (list
  (dtao:block-group
   ;; Override colors for this group
   #:bg-color "#FF0000"
   #:fg-color "#FFFFFF"
   #:content '(error-message))))

(dtao:set-renderer
  (lambda (monitor current-state)
    (if (and (dtao:monitor-focused monitor)
             (eq? current-state 'error))
        'error-layout
        'default-layout)))
