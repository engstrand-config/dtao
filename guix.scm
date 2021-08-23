(use-modules (dwl-guile packages)
             (guix gexp)
             (guix utils)
             (guix packages)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages guile)
             (gnu packages groff)
             (gnu packages xdisorg)
             (gnu packages fontutils)
             (gnu packages pkg-config)
             ((guix licenses) #:prefix license:))

(define this-directory
  (dirname (current-filename)))

(define source
  (local-file this-directory
              #:recursive? #t
              #:select? (git-predicate this-directory)))

(package
  (name "dtao-devel")
  (version "0.1")
  (home-page "https://github.com/engstrand-config/dtao")
  (source source)
  (build-system gnu-build-system)
  (native-inputs
    `(("pkg-config" ,pkg-config)))
  (inputs
    `(("guile-3.0" ,guile-3.0)
      ("wlroots-0.13.0" ,wlroots-0.13.0)
      ("fcft" ,fcft)
      ("pixman" ,pixman)
      ("groff" ,groff)
      ("ronn" ,ronn-ng)))
  (arguments
    `(#:tests? #f
      #:make-flags
      (list
        (string-append "CC=" ,(cc-for-target))
        (string-append "PREFIX=" (assoc-ref %outputs "out")))
      #:phases
      (modify-phases %standard-phases
                     (delete 'configure))))
  (license (list license:gpl3+ license:expat license:cc0))
  (synopsis "dtao - dzen for Wayland")
  (description
    "dtao is a stdin-based general-purpose bar for Wayland,
    modeled after the venerable dzen2"))
