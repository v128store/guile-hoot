(use-modules (hoot compile)
             (ice-9 binary-ports)
             (wasm assemble))

(call-with-output-file "repl.wasm"
  (lambda (port)
    (put-bytevector port
                    (assemble-wasm
                     (compile
                      (call-with-input-file "repl.scm" read))))))
