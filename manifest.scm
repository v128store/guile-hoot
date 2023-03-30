(use-modules (guix)
             (guix packages)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages guile))

(packages->manifest (list bash coreutils grep gnu-make guile-3.0-latest))
