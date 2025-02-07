(begin 
  (use-modules 
   (ice-9 match)
   (ice-9 popen)
   (ice-9 rdelim)
   (srfi srfi-1))
  (define config 
    (quasiquote 
     (("blocks") 
      ("left-blocks" 
       (("render" lambda () (cond ((dtao:selected-tag? 0) "^bg(#ffcc00)^fg(#191919)^p(8)1^p(8)^fg()^bg()") ((dtao:urgent-tag? 0) "^bg(#ff0000)^fg(#ffffff)^p(8)1^p(8)^fg()^bg()") ((dtao:active-tag? 0) "^bg(#323232)^fg(#ffffff)^p(8)1^p(8)^fg()^bg()") (else "^p(8)1^p(8)")))
        ("click" lambda (button) (match button (0 (dtao:view 0))))
        ("position" . "right")
        ("signal" . #f)
        ("events" . #t)
        ("interval" . 0))
       (("render" lambda () 
         (cond ((dtao:selected-tag? 1) "^bg(#ffcc00)^fg(#191919)^p(8)2^p(8)^fg()^bg()") ((dtao:urgent-tag? 1) "^bg(#ff0000)^fg(#ffffff)^p(8)2^p(8)^fg()^bg()") ((dtao:active-tag? 1) "^bg(#323232)^fg(#ffffff)^p(8)2^p(8)^fg()^bg()") (else "^p(8)2^p(8)")))
        ("click" lambda (button) (match button (0 (dtao:view 1))))
        ("position" . "right")
        ("signal" . #f)
        ("events" . #t)
        ("interval" . 0))
       (("render" lambda () 
         (cond ((dtao:selected-tag? 2) "^bg(#ffcc00)^fg(#191919)^p(8)3^p(8)^fg()^bg()") ((dtao:urgent-tag? 2) "^bg(#ff0000)^fg(#ffffff)^p(8)3^p(8)^fg()^bg()") ((dtao:active-tag? 2) "^bg(#323232)^fg(#ffffff)^p(8)3^p(8)^fg()^bg()") (else "^p(8)3^p(8)"))) 
        ("click" lambda (button) (match button (0 (dtao:view 2))))
        ("position" . "right")
        ("signal" . #f)
        ("events" . #t)
        ("interval" . 0))
       (("render" lambda () 
         (cond ((dtao:selected-tag? 3) "^bg(#ffcc00)^fg(#191919)^p(8)4^p(8)^fg()^bg()") ((dtao:urgent-tag? 3) "^bg(#ff0000)^fg(#ffffff)^p(8)4^p(8)^fg()^bg()") ((dtao:active-tag? 3) "^bg(#323232)^fg(#ffffff)^p(8)4^p(8)^fg()^bg()") (else "^p(8)4^p(8)")))
        ("click" lambda (button) (match button (0 (dtao:view 3))))
        ("position" . "right")
        ("signal" . #f)
        ("events" . #t)
        ("interval" . 0))
       (("render" lambda () 
         (cond ((dtao:selected-tag? 4) "^bg(#ffcc00)^fg(#191919)^p(8)5^p(8)^fg()^bg()") ((dtao:urgent-tag? 4) "^bg(#ff0000)^fg(#ffffff)^p(8)5^p(8)^fg()^bg()") ((dtao:active-tag? 4) "^bg(#323232)^fg(#ffffff)^p(8)5^p(8)^fg()^bg()") (else "^p(8)5^p(8)")))
        ("click" lambda (button) (match button (0 (dtao:view 4))))
        ("position" . "right")
        ("signal" . #f)
        ("events" . #t)
        ("interval" . 0))
       (("render" lambda () 
         (cond ((dtao:selected-tag? 5) "^bg(#ffcc00)^fg(#191919)^p(8)6^p(8)^fg()^bg()") ((dtao:urgent-tag? 5) "^bg(#ff0000)^fg(#ffffff)^p(8)6^p(8)^fg()^bg()") ((dtao:active-tag? 5) "^bg(#323232)^fg(#ffffff)^p(8)6^p(8)^fg()^bg()") (else "^p(8)6^p(8)")))
        ("click" lambda (button) (match button (0 (dtao:view 5))))
        ("position" . "right")
        ("signal" . #f)
        ("events" . #t)
        ("interval" . 0))
       (("render" lambda () 
         (cond ((dtao:selected-tag? 6) "^bg(#ffcc00)^fg(#191919)^p(8)7^p(8)^fg()^bg()") ((dtao:urgent-tag? 6) "^bg(#ff0000)^fg(#ffffff)^p(8)7^p(8)^fg()^bg()") ((dtao:active-tag? 6) "^bg(#323232)^fg(#ffffff)^p(8)7^p(8)^fg()^bg()") (else "^p(8)7^p(8)")))
        ("click" lambda (button) (match button (0 (dtao:view 6))))
        ("position" . "right")
        ("signal" . #f)
        ("events" . #t)
        ("interval" . 0))
       (("render" lambda () 
         (cond ((dtao:selected-tag? 7) "^bg(#ffcc00)^fg(#191919)^p(8)8^p(8)^fg()^bg()") ((dtao:urgent-tag? 7) "^bg(#ff0000)^fg(#ffffff)^p(8)8^p(8)^fg()^bg()") ((dtao:active-tag? 7) "^bg(#323232)^fg(#ffffff)^p(8)8^p(8)^fg()^bg()") (else "^p(8)8^p(8)")))
        ("click" lambda (button) (match button (0 (dtao:view 7))))
        ("position" . "right")
        ("signal" . #f)
        ("events" . #t)
        ("interval" . 0))
       (("render" lambda () 
         (cond ((dtao:selected-tag? 8) "^bg(#ffcc00)^fg(#191919)^p(8)9^p(8)^fg()^bg()") ((dtao:urgent-tag? 8) "^bg(#ff0000)^fg(#ffffff)^p(8)9^p(8)^fg()^bg()") ((dtao:active-tag? 8) "^bg(#323232)^fg(#ffffff)^p(8)9^p(8)^fg()^bg()") (else "^p(8)9^p(8)")))
        ("click" lambda (button) (match button (0 (dtao:view 8))))
        ("position" . "right")
        ("signal" . #f)
        ("events" . #t)
        ("interval" . 0))
       (("render" lambda () (string-append "^p(4)" (dtao:get-layout)))
        ("click" lambda (button) (dtao:next-layout))
        ("position" . "right")
        ("signal" . #f)
        ("events" . #t)
        ("interval" . 0)))
      ("center-blocks" 
       (("render" lambda () (dtao:title))
        ("click" . #f)
        ("position" . "right")
        ("signal" . #f)
        ("events" . #t)
        ("interval" . 0)))
      ("right-blocks" 
       (("render" lambda () (strftime "%A, %d %b (w.%V) %T" (localtime (current-time))))
        ("click" . #f)
        ("position" . "right")
        ("signal" . #f)
        ("events" . #f)
        ("interval" . 1)))
      ("height" . #f)
      ("font" . "monospace:style=bold:size=12")
      ("background-color" . "111111AA")
      ("border-color" . "333333FF")
      ("foreground-color" . "FFFFFFFF")
      ("border-px" . #f)
      ("exclusive" . #t)
      ("bottom" . #f)
      ("padding-top" . 2)
      ("padding-bottom" . 2)
      ("padding-left" . 8)
      ("padding-right" . 8)
      ("adjust-width" . #f)
      ("delimiter" . #f)
      ("block-spacing" . 0)
      ("layer" . LAYER-BOTTOM)))))
