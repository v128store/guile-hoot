(use-modules (hoot compile)
             (rnrs bytevectors)
             (wasm assemble))

(define (write-size-metric name bytes)
  (format #t "wasm_binary_size{name=~s} ~a\n" name (round (/ bytes 1024))))

(define* (measure-size name #:key import-abi? export-abi?)
  (define src 42) ; very simple program
  (write-size-metric name
                     (bytevector-length
                      (assemble-wasm
                       (compile src
                                #:import-abi? import-abi?
                                #:export-abi? export-abi?)))))


(display "
# TYPE wasm_binary_size summary
# UNIT wasm_binary_size KiB
# HELP wasm_binary_size Size of Wasm binary
")
(measure-size "import-abi" #:import-abi? #t)
(measure-size "main")
(measure-size "main-export-abi" #:export-abi? #t)
