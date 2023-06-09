(use-modules (dwl-guile packages)
             (guix gexp)
             (guix utils)
             (guix packages)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages wm)
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
  (name "dtao-guile-devel")
  (version "1.0")
  (home-page "https://github.com/engstrand-config/dtao-guile")
  (source source)
  (build-system gnu-build-system)
  (native-inputs (list pkg-config))
  (inputs (list guile-3.0
                wlroots
                fcft
                pixman
                groff
                ronn-ng))
  (arguments
    `(#:tests? #f
      #:make-flags
      (list
        (string-append "CC=" ,(cc-for-target))
        (string-append "PREFIX=" (assoc-ref %outputs "out")))
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (replace 'install
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                    (install-file "dtao" bin)
                    (rename-file (string-append bin "/dtao")
                                 (string-append bin "/dtao-guile-devel"))
                    #t))))))
  (license (list license:gpl3+ license:expat license:cc0))
  (synopsis "dtao-guile - bar for Wayland using Guile")
  (description "dtao-guile is a Wayland bar that is fully configured
in Guile, both general configuration, as well as blocks."))
