(use-modules (guix)
             (guix packages)
             (ice-9 match)
             (srfi srfi-1))

(define hoot-inputs
  (filter-map
   (match-lambda
     ((_ (? package? package) output) (list package output))
     ((_ (? package? package)) package)
     (_ #f)) ; ignore source inputs, etc.
   (package-development-inputs
    (load (string-append (dirname (current-filename)) "/guix.scm")))))

(packages->manifest hoot-inputs)
