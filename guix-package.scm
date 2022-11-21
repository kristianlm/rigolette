(use-modules (guix packages)
             ((guix licenses) #:prefix license:)
             (guix gexp)
             (guix download)
             (guix build-system chicken)
             (gnu packages chicken))

(package
  (name "rigolette")
  (version "0.0.1")
  (source (local-file "./" "chicken-rigolette-source"
                      #:recursive? #t
                      #:select?
                      (lambda (file stat)
                        (not (equal? ".git" (basename file))))))
  (build-system chicken-build-system)
  (propagated-inputs (list chicken-srfi-1))
  (arguments
   `(#:egg-name "rigolette"
     #:phases
     (modify-phases %standard-phases
       (add-after 'setup-chicken-environment
           'setup-chicken-environment-more
         (lambda* (#:key outputs #:allow-other-keys)
           (setenv "CHICKEN_INSTALL_PREFIX"
                   (assoc-ref outputs "out")))))))
  (synopsis "TODO")
  (description "TODO")
  (license license:bsd-3)
  (home-page "TODO"))
