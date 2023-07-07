((nil
  .
  ;; Setup Geiser to wrap Guile in pre-inst-env.
  ((eval . (let ((rootdir (expand-file-name
                           (file-name-as-directory
                            (project-root
                             (project-current))))))
             (setq-local geiser-guile-binary
                         (list (concat rootdir "pre-inst-env") "guile")))))))
