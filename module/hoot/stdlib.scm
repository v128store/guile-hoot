;;; Standard library for Hoot runtime
;;; Copyright (C) 2023 Igalia, S.L.
;;; Copyright (C) 2023 Robin Templeton <robin@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Standard runtime routines for Hoot WebAssembly runtime.
;;;
;;; Code:

(define-module (hoot stdlib)
  #:use-module (wasm wat)
  #:use-module (ice-9 receive)
  #:export (compute-stdlib))

(define (arith-cond . clauses)
  (receive (type clauses)
      (if (and (pair? clauses) (pair? (car clauses)) (pair? (caar clauses)))
          (values '(ref eq) clauses)
          (values (car clauses) (cdr clauses)))
   (if (null? clauses)
       '(unreachable)
       (let* ((clause1 (car clauses))
              (cond1 (car clause1))
              (res1 (cdr clause1)))
         (if (eq? cond1 'else)
             `(block ,type ,@res1)
             `(if ,type ,cond1
                  (then ,@res1)
                  (else ,(apply arith-cond type (cdr clauses)))))))))

(define (call-fmath fn . args)
  `(struct.new $flonum
               (i32.const 0)
               (call ,fn
                     ,@(map (lambda (arg)
                              `(struct.get $flonum
                                           $val
                                           (call $inexact ,arg)))
                            args))))

(define (compute-stdlib import-abi?)
  (define (maybe-import id)
    (if import-abi?
        `(,id (import "abi" ,(symbol->string id)))
        `(,id)))

  (define maybe-init-proc
    (if import-abi?
        '()
        '((struct.new $proc (i32.const 0)
                      (ref.func $invalid-continuation)))))
  (define maybe-init-i31-zero
    (if import-abi?
        '()
        '((ref.i31 (i32.const 0)))))
  (define maybe-init-i32-zero
    (if import-abi?
        '()
        '((i32.const 0))))
  (define maybe-init-hash-table
    (if import-abi?
        '()
        '((struct.new $hash-table (i32.const 0)
                      (i32.const 0)
                      (array.new $raw-scmvector (ref.i31 (i32.const 13))
                                 (i32.const 47))))))

  (wat->wasm
   `((type $kvarargs
           (func (param $nargs i32)
                 (param $arg0 (ref eq))
                 (param $arg1 (ref eq))
                 (param $arg2 (ref eq))))

     (type $raw-bitvector (array (mut i32)))
     (type $raw-bytevector (array (mut i8)))
     (type $raw-scmvector (array (mut (ref eq))))

     (rec
      (type $heap-object
            (sub
             (struct
              (field $hash (mut i32)))))

      (type $extern-ref
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $val (ref extern)))))

      (type $heap-number
            (sub $heap-object
              (struct
               (field $hash (mut i32)))))
      (type $bignum
            (sub $heap-number
              (struct
               (field $hash (mut i32))
               (field $val (ref extern)))))
      (type $flonum
            (sub $heap-number
              (struct
               (field $hash (mut i32))
               (field $val f64))))
      (type $complex
            (sub $heap-number
              (struct
               (field $hash (mut i32))
               (field $real f64)
               (field $imag f64))))
      (type $fraction
            (sub $heap-number
              (struct
               (field $hash (mut i32))
               (field $num (ref eq))
               (field $denom (ref eq)))))

      (type $pair
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $car (mut (ref eq)))
               (field $cdr (mut (ref eq))))))
      (type $mutable-pair
            (sub $pair
              (struct
               (field $hash (mut i32))
               (field $car (mut (ref eq)))
               (field $cdr (mut (ref eq))))))
      (type $vector
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $vals (ref $raw-scmvector)))))
      (type $mutable-vector
            (sub $vector
              (struct
               (field $hash (mut i32))
               (field $vals (ref $raw-scmvector)))))
      (type $bytevector
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $vals (ref $raw-bytevector)))))
      (type $mutable-bytevector
            (sub $bytevector
              (struct
               (field $hash (mut i32))
               (field $vals (ref $raw-bytevector)))))
      (type $bitvector
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $len i32)
               (field $vals (ref $raw-bitvector)))))
      (type $mutable-bitvector
            (sub $bitvector
              (struct
               (field $hash (mut i32))
               (field $len i32)
               (field $vals (ref $raw-bitvector)))))
      (type $string
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $str (mut (ref string))))))
      (type $mutable-string
            (sub $string
              (struct
               (field $hash (mut i32))
               (field $str (mut (ref string))))))
      (type $proc
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $func (ref $kvarargs)))))
      (type $symbol
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $name (ref $string)))))
      (type $keyword
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $name (ref $symbol)))))
      (type $variable
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $val (mut (ref eq))))))
      (type $atomic-box
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $val (mut (ref eq))))))
      (type $hash-table
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $size (mut i32))
               (field $buckets (ref $raw-scmvector)))))
      (type $weak-table
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $val (ref extern)))))
      (type $fluid
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $init (ref eq)))))
      (type $dynamic-state
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $fluids (ref $hash-table)))))
      (type $syntax
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $expr (ref eq))
               (field $wrap (ref eq))
               (field $module (ref eq))
               (field $source (ref eq)))))
      (type $port
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $open? (mut (ref eq)))  ;; #f | #t
               (field $read (ref eq))         ;; #f | (bv, start, count) -> size
               (field $write (ref eq))        ;; #f | (bv, start, count) -> size
               (field $input-waiting? (ref eq))   ;; #f | () -> bool
               (field $seek (ref eq))         ;; #f | (offset, whence) -> offset
               (field $close (ref eq))        ;; #f | () -> ()
               (field $truncate (ref eq))     ;; #f | (length) -> ()
               (field $repr (ref $string))
               (field $file-name (mut (ref eq)))      ;; #f | string
               (field $position (ref $mutable-pair))  ;; (line . column)
               (field $read-buf (mut (ref eq)))   ;; #f | #(bv cur end has-eof?)
               (field $write-buf (mut (ref eq)))  ;; #f | #(bv cur end)
               (field $read-buffering (mut (ref eq))) ;; #f | [1,size,1<<29)
               (field $r/w-random-access? (ref eq))   ;; #f | #t
               (field $private-data (ref eq)))))  ;; whatever
      (type $struct
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               ;; Vtable link is mutable so that we can tie the knot for top
               ;; types.
               (field $vtable (mut (ref null $vtable))))))
      (type $vtable
            (sub $struct
              (struct
               (field $hash (mut i32))
               (field $vtable (mut (ref null $vtable)))
               (field $field0 (mut (ref eq)))
               (field $field1 (mut (ref eq)))
               (field $field2 (mut (ref eq)))
               (field $field3 (mut (ref eq))))))
      (type $parameter
            (sub $proc
              (struct
               (field $hash (mut i32))
               (field $func (ref $kvarargs))
               (field $fluid (ref $fluid))
               (field $convert (ref $proc)))))

      (type $dyn (sub (struct)))
      (type $dynwind
            (sub $dyn
              (struct
               (field $wind (ref $proc))
               (field $unwind (ref $proc)))))
      (type $dynprompt
            (sub $dyn
              (struct
               (field $raw-sp i32)
               (field $scm-sp i32)
               (field $ret-sp i32)
               (field $unwind-only? i8)
               (field $tag (ref eq))
               (field $handler (ref $kvarargs)))))
      (type $dynfluid
            (sub $dyn
              (struct
               (field $fluid (ref $fluid))
               (field $val (mut (ref eq))))))
      (type $dynstate
            (sub $dyn
              (struct
               (field $fluids (mut (ref $hash-table)))))))

     (type $raw-retvector (array (mut (ref $kvarargs))))
     (type $raw-dynvector (array (mut (ref $dyn))))
     (type $cont
           (sub $proc
             (struct
              (field $hash (mut i32))
              (field $func (ref $kvarargs))
              (field $prompt (ref $dynprompt))
              (field $raw-stack (ref $raw-bytevector))
              (field $scm-stack (ref $raw-scmvector))
              (field $ret-stack (ref $raw-retvector))
              (field $dyn-stack (ref $raw-dynvector)))))

     ;; FIXME: This is a throwaway definition.  Really exceptions need
     ;; to be based on top of records, but we won't have records
     ;; implemented for the MVP.
     (type $simple-exception
           (sub $vector
             (struct
              (field $hash (mut i32))
              ;; #(key subr message irritants)
              (field $vals (ref $raw-scmvector)))))

     (func $make-throw-exn (param $key (ref $symbol)) (param $args (ref eq))
           (result (ref $simple-exception))
           (struct.new $simple-exception (i32.const 0)
                       (array.new_fixed $raw-scmvector 4
                                        (local.get $key)
                                        (ref.i31 (i32.const 1))
                                        (ref.i31 (i32.const 1))
                                        (local.get $args))))
     (func $make-throw/value-exn
           (param $key (ref $symbol))
           (param $subr (ref eq))
           (param $message (ref $string))
           (param $args (ref eq))
           (result (ref $simple-exception))
           (struct.new $simple-exception (i32.const 0)
                       (array.new_fixed $raw-scmvector 4
                                        (local.get $key)
                                        (local.get $subr)
                                        (local.get $message)
                                        (local.get $args))))

     (func $raise-exception (param $exn (ref eq))
           (return_call_ref $kvarargs
                            (i32.const 2)
                            (global.get $raise-exception)
                            (local.get $exn)
                            (ref.i31 (i32.const 1))
                            (struct.get $proc $func (global.get $raise-exception))))

     (func $string->bignum (import "rt" "bignum_from_string")
           (param (ref string))
           (result (ref extern)))
     (func $bignum-from-i32 (import "rt" "bignum_from_i32")
           (param i32)
           (result (ref extern)))
     (func $bignum-from-i64 (import "rt" "bignum_from_i64")
           (param i64)
           (result (ref extern)))
     (func $bignum-from-u64 (import "rt" "bignum_from_u64")
           (param i64)
           (result (ref extern)))
     (func $bignum-is-i64 (import "rt" "bignum_is_i64")
           (param (ref extern))
           (result i32))
     (func $bignum-is-u64 (import "rt" "bignum_is_u64")
           (param (ref extern))
           (result i32))
     (func $bignum-get-i64 (import "rt" "bignum_get_i64")
           (param (ref extern))
           (result i64))

     (func $bignum-add (import "rt" "bignum_add")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-add-i32 (import "rt" "bignum_add")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-sub (import "rt" "bignum_sub")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-sub-i32 (import "rt" "bignum_sub")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-sub-i32-i32 (import "rt" "bignum_sub")
           (param i32)
           (param i32)
           (result (ref extern)))
     (func $bignum-mul (import "rt" "bignum_mul")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-mul-i32 (import "rt" "bignum_mul")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-lsh (import "rt" "bignum_lsh")
           (param (ref extern))
           (param i64)
           (result (ref extern)))
     (func $i32-lsh (import "rt" "bignum_lsh")
           (param i32)
           (param i64)
           (result (ref extern)))
     (func $bignum-rsh (import "rt" "bignum_rsh")
           (param (ref extern))
           (param i64)
           (result (ref extern)))
     (func $bignum-quo (import "rt" "bignum_quo")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-rem (import "rt" "bignum_rem")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-mod (import "rt" "bignum_mod")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-gcd (import "rt" "bignum_gcd")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))

     (func $bignum-logand-i32 (import "rt" "bignum_logand")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-logand-bignum (import "rt" "bignum_logand")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-logior-i32 (import "rt" "bignum_logior")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-logior-bignum (import "rt" "bignum_logior")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-logxor-i32 (import "rt" "bignum_logxor")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-logxor-bignum (import "rt" "bignum_logxor")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $i32-logsub-bignum (import "rt" "bignum_logsub")
           (param i32)
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-logsub-i32 (import "rt" "bignum_logsub")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-logsub-bignum (import "rt" "bignum_logsub")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))

     (func $lt-fix-big (import "rt" "bignum_lt")
           (param i32)
           (param (ref extern))
           (result i32))
     (func $lt-big-fix (import "rt" "bignum_lt")
           (param (ref extern))
           (param i32)
           (result i32))
     (func $lt-big-big (import "rt" "bignum_lt")
           (param (ref extern))
           (param (ref extern))
           (result i32))
     (func $lt-big-flo (import "rt" "bignum_lt")
           (param (ref extern))
           (param f64)
           (result i32))
     (func $lt-flo-big (import "rt" "bignum_lt")
           (param f64)
           (param (ref extern))
           (result i32))

     (func $le-fix-big (import "rt" "bignum_le")
           (param i32)
           (param (ref extern))
           (result i32))
     (func $le-big-fix (import "rt" "bignum_le")
           (param (ref extern))
           (param i32)
           (result i32))
     (func $le-big-big (import "rt" "bignum_le")
           (param (ref extern))
           (param (ref extern))
           (result i32))
     (func $le-big-flo (import "rt" "bignum_le")
           (param (ref extern))
           (param f64)
           (result i32))
     (func $le-flo-big (import "rt" "bignum_le")
           (param f64)
           (param (ref extern))
           (result i32))

     (func $eq-fix-big (import "rt" "bignum_eq")
           (param i32)
           (param (ref extern))
           (result i32))
     (func $eq-big-fix (import "rt" "bignum_eq")
           (param (ref extern))
           (param i32)
           (result i32))
     (func $eq-big-big (import "rt" "bignum_eq")
           (param (ref extern))
           (param (ref extern))
           (result i32))
     (func $eq-big-flo (import "rt" "bignum_eq")
           (param (ref extern))
           (param f64)
           (result i32))
     (func $eq-flo-big (import "rt" "bignum_eq")
           (param f64)
           (param (ref extern))
           (result i32))

     (func $bignum-to-f64 (import "rt" "bignum_to_f64")
           (param (ref extern))
           (result f64))

     (func $f64-is-nan (import "rt" "f64_is_nan")
           (param f64)
           (result i32))
     (func $f64-is-infinite (import "rt" "f64_is_infinite")
           (param f64)
           (result i32))

     (func $flonum->string (import "rt" "flonum_to_string")
           (param f64)
           (result (ref string)))

     (func $string-upcase (import "rt" "string_upcase")
           (param (ref string))
           (result (ref string)))
     (func $string-downcase (import "rt" "string_downcase")
           (param (ref string))
           (result (ref string)))

     (func $make-weak-map (import "rt" "make_weak_map")
           (result (ref extern)))
     (func $weak-map-get (import "rt" "weak_map_get")
           (param (ref extern) (ref eq))
           (result (ref eq)))
     (func $weak-map-set (import "rt" "weak_map_set")
           (param (ref extern) (ref eq) (ref eq)))
     (func $weak-map-delete (import "rt" "weak_map_delete")
           (param (ref extern) (ref eq))
           (result i32))

     ;; FIXME: These are very much temporary.
     (func $write-stdout (import "io" "write_stdout") (param (ref string)))
     (func $write-stderr (import "io" "write_stderr") (param (ref string)))
     (func $read-stdin (import "io" "read_stdin") (result (ref string)))

     (func $fsqrt (import "rt" "fsqrt") (param f64) (result f64))
     (func $fsin (import "rt" "fsin") (param f64) (result f64))
     (func $fcos (import "rt" "fcos") (param f64) (result f64))
     (func $ftan (import "rt" "ftan") (param f64) (result f64))
     (func $fasin (import "rt" "fasin") (param f64) (result f64))
     (func $facos (import "rt" "facos") (param f64) (result f64))
     (func $fatan (import "rt" "fatan") (param f64) (result f64))
     (func $fatan2 (import "rt" "fatan2") (param f64 f64) (result f64))
     (func $flog (import "rt" "flog") (param f64) (result f64))
     (func $fexp (import "rt" "fexp") (param f64) (result f64))

     (func $jiffies-per-second (import "rt" "jiffies_per_second") (result i32))
     (func $current-jiffy (import "rt" "current_jiffy") (result i64))
     (func $current-second (import "rt" "current_second") (result f64))

     (func $die (import "rt" "die")
           (param (ref string) (ref eq)))

     (func $debug-str (import "debug" "debug_str")
           (param (ref string)))
     (func $debug-str-i32 (import "debug" "debug_str_i32")
           (param (ref string) i32))
     (func $debug-str-scm (import "debug" "debug_str_scm")
           (param (ref string) (ref eq)))

     (func $die0 (param $reason (ref string))
           (call $die (local.get 0) (ref.i31 (i32.const 1))))

     ;; Thomas Wang's integer hasher, from
     ;; http://www.cris.com/~Ttwang/tech/inthash.htm.
     (func $integer-hash (param $v i32) (result i32)
           (local.set $v (i32.xor (i32.xor (local.get $v) (i32.const 61))
                                  (i32.shr_u (local.get $v) (i32.const 16))))
           (local.set $v (i32.add (local.get $v)
                                  (i32.shl (local.get $v) (i32.const 3))))
           (local.set $v (i32.xor (local.get $v)
                                  (i32.shr_u (local.get $v) (i32.const 4))))
           (local.set $v (i32.mul (local.get $v)
                                  (i32.const #x27d4eb2d)))
           (i32.xor (local.get $v)
                    (i32.shr_u (local.get $v) (i32.const 15))))

     (func $finish-heap-object-hash (param $hash i32) (result i32)
           (local.set $hash (call $integer-hash (local.get $hash)))
           (if i32 (local.get $hash)
               (then (local.get $hash))
               (else (call $integer-hash (i32.const 42)))))

     (global $hashq-counter (mut i32) (i32.const 0))
     (func $immediate-hashq (param $v (ref i31)) (result i32)
           (call $integer-hash (i31.get_u (local.get $v))))
     (func $heap-object-hashq (param $v (ref $heap-object)) (result i32)
           (local $tag i32)
           (local.set $tag (struct.get $heap-object $hash (local.get $v)))
           (loop $init-if-zero
             (block
              $done
              (br_if $done (local.get $tag))
              (global.set $hashq-counter
                          (i32.sub (global.get $hashq-counter) (i32.const 1)))
              (struct.set $heap-object $hash (local.get $v)
                          (local.tee $tag (call $integer-hash
                                                (global.get $hashq-counter))))
              ;; Check and retry if result is zero.
              (br $init-if-zero)))
           (local.get $tag))
     (func $hashq (param $v (ref eq)) (result i32)
           (if i32
               (ref.test i31 (local.get $v))
               (then
                (return_call $immediate-hashq
                             (ref.cast i31 (local.get $v))))
               (else
                (return_call $heap-object-hashq
                             (ref.cast $heap-object (local.get $v))))))

     (func $grow-raw-stack
           ;; Grow the stack by at least 50% and at least the needed
           ;; space.  Trap if we fail to grow.
           ;; additional_size = (current_size >> 1) | needed_size
           (if (i32.eq
                (memory.grow
                 $raw-stack
                 (i32.or (i32.shr_u (memory.size $raw-stack) (i32.const 1))
                         ;; Wasm pages are 64 kB.
                         (i32.sub (i32.add (i32.shr_u (global.get $raw-sp)
                                                      (i32.const 16))
                                           (i32.const 1))
                                  (memory.size $raw-stack))))
                (i32.const -1))
               (then (call $die0 (string.const "$grow-raw-stack")) (unreachable))))
     (func $maybe-grow-raw-stack
           (if (i32.lt_u (i32.shl (memory.size $raw-stack) (i32.const 16))
                         (global.get $raw-sp))
               (then (call $grow-raw-stack))))

     (func $grow-scm-stack
           ;; Grow as in $grow-raw-stack.
           (if (i32.eq
                (table.grow $scm-stack
                            (ref.i31 (i32.const 0))
                            (i32.or (i32.shr_u (table.size $scm-stack)
                                               (i32.const 1))
                                    (i32.sub (global.get $scm-sp)
                                             (table.size $scm-stack))))
                (i32.const -1))
               (then
                (call $die0 (string.const "$grow-scm-stack"))
                (unreachable))))
     (func $maybe-grow-scm-stack
           (if (i32.lt_u (table.size $scm-stack) (global.get $scm-sp))
               (then (call $grow-scm-stack))))

     (func $invalid-continuation (type $kvarargs)
           (call $die0 (string.const "$invalid-continuation"))
           (unreachable))
     (func $grow-ret-stack
           ;; Grow as in $grow-raw-stack.
           (if (i32.eq (table.grow $ret-stack
                            (ref.func $invalid-continuation)
                            (i32.or (i32.shr_u (table.size $ret-stack)
                                               (i32.const 1))
                                    (i32.sub (global.get $ret-sp)
                                             (table.size $ret-stack))))
                       (i32.const -1))
               (then
                (call $die0 (string.const "$grow-ret-stack"))
                (unreachable))))
     (func $maybe-grow-ret-stack
           (if (i32.lt_u (table.size $ret-stack) (global.get $ret-sp))
               (then (call $grow-ret-stack))))

     (func $grow-dyn-stack
           ;; Grow as in $grow-ret-stack.
           (if (i32.eq (table.grow $dyn-stack
                                   (ref.null $dyn)
                                   (i32.or (i32.shr_u (table.size $dyn-stack)
                                                      (i32.const 1))
                                           (i32.sub (global.get $dyn-sp)
                                                    (table.size $dyn-stack))))
                       (i32.const -1))
               (then
                (call $die0 (string.const "$grow-dyn-stack"))
                (unreachable))))
     (func $maybe-grow-dyn-stack
           (if (i32.lt_u (table.size $dyn-stack) (global.get $dyn-sp))
               (then (call $grow-dyn-stack))))

     (func $wrong-num-args (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (call $die (string.const "wrong-number-of-args") (local.get $arg0))
           (unreachable))

     (func $collect-rest-args (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (param $npositional i32)
           (result (ref eq))
           (local $ret (ref eq))
           (local.set $ret (ref.i31 (i32.const 13))) ;; null
           (block
            $done
            (block
             $nargs1
             (block
              $nargs2
              (block
               $nargs3
               (block
                $nargs4
                (block
                 $nargs5
                 (block
                  $nargs6
                  (block
                   $nargs7
                   (block
                    $nargs8
                    (block
                     $nargsN
                     (br_table $done
                               $nargs1
                               $nargs2
                               $nargs3
                               $nargs4
                               $nargs5
                               $nargs6
                               $nargs7
                               $nargs8
                               $nargsN
                               (local.get $nargs)))
                    (loop $lp
                      (if (i32.gt_u (local.get $nargs) (i32.const 8))
                          (then
                           (br_if $done (i32.le_u (local.get $nargs)
                                                  (local.get $npositional)))
                           (local.set
                            $ret
                            (struct.new
                             $pair
                             (i32.const 0)
                             (ref.as_non_null
                              (table.get
                               $argv
                               (i32.sub
                                (local.tee $nargs
                                           (i32.sub (local.get $nargs) (i32.const 1)))
                                (i32.const 8))))
                             (local.get $ret)))
                           (br $lp)))))
                   (br_if $done (i32.le_u (i32.const 8) (local.get $npositional)))
                   (local.set $ret
                              (struct.new $pair (i32.const 0)
                                          (global.get $arg7) (local.get $ret))))
                  (br_if $done (i32.le_u (i32.const 7) (local.get $npositional)))
                  (local.set $ret
                             (struct.new $pair (i32.const 0)
                                         (global.get $arg6) (local.get $ret))))
                 (br_if $done (i32.le_u (i32.const 6) (local.get $npositional)))
                 (local.set $ret
                            (struct.new $pair (i32.const 0)
                                        (global.get $arg5) (local.get $ret))))
                (br_if $done (i32.le_u (i32.const 5) (local.get $npositional)))
                (local.set $ret
                           (struct.new $pair (i32.const 0)
                                       (global.get $arg4) (local.get $ret))))
               (br_if $done (i32.le_u (i32.const 4) (local.get $npositional)))
               (local.set $ret
                          (struct.new $pair (i32.const 0)
                                      (global.get $arg3) (local.get $ret))))
              (br_if $done (i32.le_u (i32.const 3) (local.get $npositional)))
              (local.set $ret
                         (struct.new $pair (i32.const 0)
                                     (local.get $arg2) (local.get $ret))))
             (br_if $done (i32.le_u (i32.const 2) (local.get $npositional)))
             (local.set $ret
                        (struct.new $pair (i32.const 0)
                                    (local.get $arg1) (local.get $ret)))
             )
            (br_if $done (i32.le_u (i32.const 1) (local.get $npositional)))
            (local.set $ret
                       (struct.new $pair (i32.const 0)
                                   (local.get $arg0) (local.get $ret))))
           (local.get $ret))

     (func $values (param $nargs i32) (param $arg0 (ref eq))
           (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (block
            $done
            (local.set $arg0 (local.get $arg1))
            (local.set $arg1 (local.get $arg2))
            (br_if $done (i32.le_u (local.get $nargs) (i32.const 3)))
            (local.set $arg2 (global.get $arg3))
            (global.set $arg3 (global.get $arg4))
            (global.set $arg4 (global.get $arg5))
            (global.set $arg5 (global.get $arg6))
            (global.set $arg6 (global.get $arg7))
            (br_if $done (i32.le_u (local.get $nargs) (i32.const 8)))
            (global.set $arg7 (ref.as_non_null (table.get $argv (i32.const 0))))
            (table.copy $argv $argv (i32.const 0) (i32.const 1)
                        (i32.sub (local.get $nargs) (i32.const 9))))
           (i32.sub (local.get $nargs) (i32.const 1))
           (local.get $arg0)
           (local.get $arg1)
           (local.get $arg2)
           (global.set $ret-sp (i32.sub (global.get $ret-sp) (i32.const 1)))
           (global.get $ret-sp)
           (table.get $ret-stack)
           (return_call_ref $kvarargs))
     (global $values-primitive (ref eq)
             (struct.new $proc (i32.const 0) (ref.func $values)))

     (func $make-hash-table (result (ref $hash-table))
           (struct.new $hash-table (i32.const 0) (i32.const 0)
                       (array.new $raw-scmvector
                                  (ref.i31 (i32.const 13)) (i32.const 47))))

     (func $hashq-lookup (param $tab (ref $hash-table)) (param $k (ref eq))
           (result (ref null $pair))
           (local $idx i32)
           (local $buckets (ref $raw-scmvector))
           (local $chain (ref eq))
           (local $head (ref $pair))
           (local $link (ref $pair))
           (local.set $buckets
                      (struct.get $hash-table $buckets (local.get $tab)))
           (local.set $idx
                      (i32.rem_u (call $hashq (local.get $k))
                                 (array.len (local.get $buckets))))
           (local.set $chain
                      (array.get $raw-scmvector
                                 (local.get $buckets) (local.get $idx)))
           (loop $lp
             (if (i32.eqz (ref.test $pair (local.get $chain)))
                 (then (return (ref.null $pair)))
                 (else
                  (local.set $link (ref.cast $pair (local.get $chain)))
                  (local.set $head
                             (ref.cast $pair
                                       (struct.get $pair $car
                                                   (local.get $link))))
                  (if (ref.eq (struct.get $pair $car (local.get $head))
                              (local.get $k))
                      (then
                       (return (local.get $head)))
                      (else
                       (local.set $chain
                                  (struct.get $pair $cdr (local.get $link)))
                       (br $lp))))))
           (unreachable))

     (func $hashq-insert (param $tab (ref $hash-table)) (param $k (ref eq))
           (param $v (ref eq))
           (local $idx i32)
           (local $buckets (ref $raw-scmvector))
           (local.set $buckets (struct.get $hash-table $buckets (local.get $tab)))
           (local.set $idx (i32.rem_u (call $hashq (local.get $k))
                                      (array.len (local.get $buckets))))
           (array.set
            $raw-scmvector
            (local.get $buckets) (local.get $idx)
            (struct.new
             $pair (i32.const 0)
             (struct.new $pair (i32.const 0) (local.get $k) (local.get $v))
             (array.get $raw-scmvector (local.get $buckets) (local.get $idx))))
           (struct.set $hash-table $size
                       (local.get $tab)
                       (i32.add (struct.get $hash-table $size (local.get $tab))
                                (i32.const 1))))

     (func $hashq-ref (param $tab (ref $hash-table)) (param $k (ref eq))
           (param $default (ref eq))
           (result (ref eq))
           (local $handle (ref null $pair))
           (local.set $handle
                      (call $hashq-lookup (local.get $tab) (local.get $k)))
           (if (ref eq)
               (ref.is_null (local.get $handle))
               (then (local.get $default))
               (else (struct.get $pair $cdr (local.get $handle)))))
     (func $hashq-update (param $tab (ref $hash-table)) (param $k (ref eq))
           (param $v (ref eq)) (param $default (ref eq))
           (result (ref eq))
           (local $handle (ref null $pair))
           (local.set $handle
                      (call $hashq-lookup (local.get $tab) (local.get $k)))
           (if (ref eq)
               (ref.is_null (local.get $handle))
               (then
                (call $hashq-insert (local.get $tab) (local.get $k)
                      (local.get $v))
                (local.get $default))
               (else
                (struct.get $pair $cdr (local.get $handle))
                (struct.set $pair $cdr (local.get $handle)
                            (local.get $v)))))
     (func $hashq-set! (param $tab (ref $hash-table)) (param $k (ref eq))
           (param $v (ref eq))
           (call $hashq-update (local.get $tab) (local.get $k)
                 (local.get $v) (ref.i31 (i32.const 0)))
           (drop))

     ;; A specialized hash table, because it's not a hashq lookup.
     (type $symtab-entry
           (struct (field $sym (ref $symbol))
                   (field $next (ref null $symtab-entry))))
     (type $symtab (array (mut (ref null $symtab-entry))))
     (global $the-symtab (ref $symtab)
             (array.new $symtab (ref.null $symtab-entry) (i32.const 47)))

     ,(cond
       (import-abi?
        '(func $intern-symbol! (import "abi" "$intern-symbol!")
               (param $sym (ref $symbol)) (result (ref $symbol))))
       (else
        '(func $intern-symbol!
               (param $sym (ref $symbol)) (result (ref $symbol))
               (local $hash i32)
               (local $idx i32)
               (local $entry (ref null $symtab-entry))
               (local.set $hash (struct.get $heap-object $hash (local.get $sym)))
               (local.set $idx (i32.rem_u (local.get $hash)
                                          (array.len (global.get $the-symtab))))
               (local.set $entry
                          (array.get $symtab (global.get $the-symtab)
                                     (local.get $idx)))
               (block
                $insert
                (loop $lp
                  (br_if $insert (ref.is_null (local.get $entry)))
                  (block
                   $next
                   (br_if $next
                          (i32.ne (struct.get $symbol $hash
                                              (struct.get $symtab-entry $sym
                                                          (local.get $entry)))
                                  (local.get $hash)))
                   (br_if $next
                          (i32.eqz
                           (string.eq
                            (struct.get $string $str
                                        (struct.get $symbol $name
                                                    (struct.get $symtab-entry $sym
                                                                (local.get $entry))))
                            (struct.get $string $str
                                        (struct.get $symbol $name
                                                    (local.get $sym))))))
                   (return (struct.get $symtab-entry $sym (local.get $entry))))
                  (local.set $entry
                             (struct.get $symtab-entry $next (local.get $entry)))
                  (br $lp)))
               (array.set $symtab (global.get $the-symtab) (local.get $idx)
                          (struct.new $symtab-entry
                                      (local.get $sym)
                                      (array.get $symtab (global.get $the-symtab)
                                                 (local.get $idx))))
               (local.get $sym))))

     ;; For now, the Java string hash function, except over codepoints
     ;; rather than WTF-16 code units.
     (func $string-hash (param $str (ref string)) (result i32)
           (local $iter (ref stringview_iter))
           (local $hash i32)
           (local $codepoint i32)
           (local.set $iter (string.as_iter (local.get $str)))
           (block $done
                  (loop $lp
                    (local.set $codepoint (stringview_iter.next (local.get $iter)))
                    (br_if $done (i32.eq (i32.const -1) (local.get $codepoint)))
                    (local.set $hash
                               (i32.add (i32.mul (local.get $hash) (i32.const 31))
                                        (local.get $codepoint)))
                    (br $lp)))
           (local.get $hash))

     (func $string->symbol (param $str (ref $string)) (result (ref $symbol))
           (call $intern-symbol!
                 (struct.new $symbol
                             (call $finish-heap-object-hash
                                   (call $string-hash
                                         (struct.get $string $str
                                                     (local.get $str))))
                             (local.get $str))))

     (global $the-kwtab (ref $hash-table)
             (struct.new $hash-table (i32.const 0) (i32.const 0)
                         (array.new $raw-scmvector
                                    (ref.i31 (i32.const 13)) (i32.const 47))))
     ,(cond
       (import-abi?
        '(func $intern-keyword! (import "abi" "$intern-keyword!")
               (param $sym (ref $keyword)) (result (ref $keyword))))
       (else
        '(func $intern-keyword! (param $kw (ref $keyword)) (result (ref $keyword))
               (local $handle (ref null $pair))
               (local.set $handle
                          (call $hashq-lookup (global.get $the-kwtab)
                                (struct.get $keyword $name (local.get $kw))))
               (if (ref $keyword)
                   (ref.is_null (local.get $handle))
                   (then
                    (call $hashq-insert (global.get $the-kwtab)
                          (struct.get $keyword $name (local.get $kw))
                          (local.get $kw))
                    (local.get $kw))
                   (else
                    (ref.cast $keyword
                              (struct.get $pair $cdr (local.get $handle))))))))

     (func $symbol->keyword (param $sym (ref $symbol)) (result (ref $keyword))
           (call $intern-keyword!
                 (struct.new $keyword
                             (call $finish-heap-object-hash
                                   (struct.get $symbol $hash (local.get $sym)))
                             (local.get $sym))))

     (func $push-dyn (param $dyn (ref $dyn))
           (local $dyn-sp i32)
           (global.set $dyn-sp
                       (i32.add (local.tee $dyn-sp (global.get $dyn-sp))
                                (i32.const 1)))
           (call $maybe-grow-dyn-stack)
           (table.set $dyn-stack (local.get $dyn-sp) (local.get $dyn)))

     (func $wind-dynstate (param $dynstate (ref $dynstate))
           (local $fluids (ref $hash-table))
           (local.set $fluids (global.get $current-fluids))
           (global.set $current-fluids
                       (struct.get $dynstate $fluids (local.get $dynstate)))
           (struct.set $dynstate $fluids (local.get $dynstate)
                       (local.get $fluids)))

     (func $push-dynamic-state (param $state (ref $dynamic-state))
           (local $dynstate (ref $dynstate))
           (call $push-dyn
                 (local.tee $dynstate
                            (struct.new $dynstate
                                        (struct.get $dynamic-state $fluids
                                                    (local.get $state)))))
           (return_call $wind-dynstate (local.get $dynstate)))

     (func $pop-dynamic-state
           (local $sp i32)
           (global.set $dyn-sp
                       (local.tee $sp (i32.sub (global.get $dyn-sp)
                                               (i32.const 1))))
           (return_call $wind-dynstate
                        (ref.as_non_null (global) (table.get $dy)) (local.get $dynstate)))

     (func $wind-dynfluid (param $dynfluid (ref $dynfluid))
           (local $fluid (ref $fluid))
           (local.set $fluid
                      (struct.get $dynfluid $fluid (local.get $dynfluid)))
           (struct.set
            $dynfluid $val
            (local.get $dynfluid)
            (call $hashq-update (global.get $current-fluids)
                  (local.get $fluid)
                  (struct.get $dynfluid $val (local.get $dynfluid))
                  (struct.get $fluid $init (local.get $fluid)))))

     (func $push-fluid (param $fluid (ref $fluid)) (param $val (ref eq))
           (local $dynfluid (ref $dynfluid))
           (local.set $dynfluid
                      (struct.new $dynfluid
                                  (local.get $fluid) (local.get $val)))
           (call $push-dyn (local.get $dynfluid))
           (call $wind-dynfluid (local.get $dynfluid)))

     (func $pop-fluid
           (local $sp i32)
           (global.set $dyn-sp
                       (local.tee $sp (i32.sub (global.get $dyn-sp)
                                               (i32.const 1))))
           (call $wind-dynfluid
                 (ref.cast $dynfluid (table.get $dyn-stack (local.get $sp)))))

     (func $fluid-ref (param $fluid (ref $fluid)) (result (ref eq))
           (call $hashq-ref (global.get $current-fluids)
                 (local.get $fluid)
                 (struct.get $fluid $init (local.get $fluid))))

     (func $fluid-ref* (param $fluid (ref $fluid)) (param $depth i32)
           (result (ref eq))
           (local $sp i32)
           (local $dyn (ref $dyn))
           (if (local.get $depth)
               (then
                (local.set $sp (global.get $dyn-sp))
                (loop $lp
                  (if (local.get $sp)
                      (then
                       (local.set $sp (i32.sub (local.get $sp (i32.const 1))))
                       (local.set $dyn (ref.as_non_null
                                        (table.get $dyn-stack (local.get $sp))))
                       (br_if $lp (i32.eqz
                                   (ref.test $dynfluid (local.get $dyn))))
                       (local.set $depth
                                  (i32.sub (local.get $depth) (i32.const 1)))
                       (br_if $lp (local.get $depth))
                       (return
                        (struct.get
                         $dynfluid $val
                         (ref.cast $dynfluid (local.get $dyn)))))
                      (else (return (ref.i31 (i32.const 1)))))))
               (else (return_call $fluid-ref (local.get $fluid))))
           (unreachable))

     (func $fluid-set! (param $fluid (ref $fluid)) (param $val (ref eq))
           (call $hashq-set! (global.get $current-fluids)
                 (local.get $fluid)
                 (local.get $val)))

     ;; FIXME: Better error handling if prompt not found.
     (func $find-prompt (param $tag (ref eq))
           (result (ref $dynprompt) i32)
           (local $dyn (ref $dyn))
           (local $prompt (ref $dynprompt))
           (local $sp i32)
           (local.set $sp (global.get $dyn-sp))
           (loop $lp
             (if (local.get $sp)
                 (then
                  (local.set $sp (i32.sub (local.get $sp) (i32.const 1)))
                  ;; FIXME: could br_on_cast_fail to $lp; need to fix
                  ;; the assembler.
                  (local.set $dyn (ref.as_non_null
                                   (table.get $dyn-stack (local.get $sp))))
                  (if (ref.test $dynprompt (local.get $dyn))
                      (then
                       (local.set $prompt
                                  (ref.cast $dynprompt (local.get $dyn)))
                       (if (ref.eq (struct.get $dynprompt $tag
                                               (local.get $prompt))
                                   (local.get $tag))
                           (then (return (local.get $prompt)
                                         (local.get $sp)))
                           (else (br $lp)))))
                  (br $lp))
                 (else
                  (call $die (string.const "prompt not found")
                        (local.get $tag)))))
           (unreachable))

     (func $rewind
           (param $raw-sp-adjust i32)
           (param $scm-sp-adjust i32)
           (param $ret-sp-adjust i32)
           (param $dyn (ref $raw-dynvector))
           (param $i i32)
           (param $args (ref eq))
           (local $d (ref $dyn))
           (local $dynwind (ref $dynwind))
           (local $dynprompt (ref $dynprompt))
           (local $dynfluid (ref $dynfluid))
           (local $dynstate (ref $dynstate))
           (local $base i32)
           (loop $lp
             (if (i32.eq (local.get $i) (array.len (local.get $dyn)))
                 (then
                  (return_call $apply (i32.const 3)
                               (global.get $apply-primitive)
                               (global.get $values-primitive)
                               (local.get $args))))
             (local.set $d (array.get $raw-dynvector
                                      (local.get $dyn)
                                      (local.get $i)))
             (block
              $next
              (if (ref.test $dynwind (local.get $d))
                  (then
                   (local.set $dynwind (ref.cast $dynwind (local.get $d)))
                   (local.set $base (global.get $raw-sp))
                   (global.set $raw-sp (i32.add (local.get $base) (i32.const 16)))
                   (global.set $scm-sp (i32.add (global.get $scm-sp) (i32.const 2)))
                   (global.set $ret-sp (i32.add (global.get $ret-sp) (i32.const 1)))
                   (call $maybe-grow-raw-stack)
                   (call $maybe-grow-scm-stack)
                   (call $maybe-grow-ret-stack)
                   (i32.store $raw-stack offset=0 (local.get $base)
                              (local.get $raw-sp-adjust))
                   (i32.store $raw-stack offset=4 (local.get $base)
                              (local.get $scm-sp-adjust))
                   (i32.store $raw-stack offset=8 (local.get $base)
                              (local.get $ret-sp-adjust))
                   (i32.store $raw-stack offset=12 (local.get $base)
                              (local.get $i))
                   (table.set $scm-stack
                              (i32.sub (global.get $scm-sp) (i32.const 2))
                              (local.get $dyn))
                   (table.set $scm-stack
                              (i32.sub (global.get $scm-sp) (i32.const 1))
                              (local.get $args))
                   (table.set $ret-stack
                              (i32.sub (global.get $ret-sp) (i32.const 1))
                              (ref.func $keep-rewinding))
                   (return_call_ref $kvarargs
                                    (i32.const 1)
                                    (struct.get $dynwind $wind
                                                (local.get $dynwind))
                                    (ref.i31 (i32.const 0))
                                    (ref.i31 (i32.const 0))
                                    (struct.get
                                     $proc $func
                                     (struct.get $dynwind $wind
                                                 (local.get $dynwind))))))
              (if (ref.test $dynprompt (local.get $d))
                  (then
                   (local.set $dynprompt (ref.cast $dynprompt (local.get $d)))
                   (local.set
                    $d
                    (struct.new
                     $dynprompt
                     (i32.add
                      (struct.get $dynprompt $raw-sp (local.get $dynprompt))
                      (local.get $raw-sp-adjust))
                     (i32.add
                      (struct.get $dynprompt $scm-sp (local.get $dynprompt))
                      (local.get $scm-sp-adjust))
                     (i32.add
                      (struct.get $dynprompt $ret-sp (local.get $dynprompt))
                      (local.get $ret-sp-adjust))
                     (struct.get_u $dynprompt $unwind-only?
                                   (local.get $dynprompt))
                     (struct.get $dynprompt $tag (local.get $dynprompt))
                     (struct.get $dynprompt $handler (local.get $dynprompt))))
                   (br $next)))
              (if (ref.test $dynfluid (local.get $d))
                  (then
                   (local.set $dynfluid (ref.cast $dynfluid (local.get $d)))
                   (call $wind-dynfluid (local.get $dynfluid))
                   (br $next)))
              (if (ref.test $dynstate (local.get $d))
                  (then
                   (local.set $dynstate (ref.cast $dynstate (local.get $d)))
                   (call $wind-dynstate (local.get $dynstate))
                   (br $next))
                  (else (unreachable))))
             (call $push-dyn (local.get $d))
             (local.set $i (i32.add (local.get $i) (i32.const 1)))
             (br $lp)))

     (func $restore-raw-stack (param $v (ref $raw-bytevector))
           (local $sp i32)
           (local $i i32)
           (local $len i32)
           (local.set $sp (global.get $raw-sp))
           (local.set $i (i32.const 0))
           (local.set $len (array.len (local.get $v)))
           (global.set $raw-sp (i32.add (local.get $sp) (local.get $len)))
           (call $maybe-grow-raw-stack)
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (i32.store8 $raw-stack
                              (i32.add (local.get $sp) (local.get $i))
                              (array.get_u $raw-bytevector
                                           (local.get $v)
                                           (local.get $i)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp)))))

     (func $restore-scm-stack (param $v (ref $raw-scmvector))
           (local $sp i32)
           (local $i i32)
           (local $len i32)
           (local.set $sp (global.get $scm-sp))
           (local.set $len (array.len (local.get $v)))
           (global.set $scm-sp (i32.add (local.get $sp) (local.get $len)))
           (call $maybe-grow-scm-stack)
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (table.set $scm-stack
                             (i32.add (local.get $sp) (local.get $i))
                             (array.get $raw-scmvector
                                        (local.get $v)
                                        (local.get $i)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp)))))

     (func $restore-ret-stack (param $v (ref $raw-retvector))
           (local $sp i32)
           (local $i i32)
           (local $len i32)
           (local.set $sp (global.get $ret-sp))
           (local.set $len (array.len (local.get $v)))
           (global.set $ret-sp (i32.add (local.get $sp) (local.get $len)))
           (call $maybe-grow-ret-stack)
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (table.set $ret-stack
                             (i32.add (local.get $sp) (local.get $i))
                             (array.get $raw-retvector
                                        (local.get $v)
                                        (local.get $i)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp)))))

     (func $compose-continuation (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (local $cont (ref $cont))
           (local $prompt (ref $dynprompt))
           (local $raw-sp-adjust i32)
           (local $scm-sp-adjust i32)
           (local $ret-sp-adjust i32)
           (local $args (ref eq))
           (local.set $cont (ref.cast $cont (local.get $arg0)))
           (local.set $prompt (struct.get $cont $prompt (local.get $cont)))
           (local.set $raw-sp-adjust
                      (i32.sub (global.get $raw-sp)
                               (struct.get $dynprompt $raw-sp
                                           (local.get $prompt))))
           (local.set $scm-sp-adjust
                      (i32.sub (global.get $scm-sp)
                               (struct.get $dynprompt $scm-sp
                                           (local.get $prompt))))
           (local.set $ret-sp-adjust
                      (i32.sub (global.get $ret-sp)
                               (struct.get $dynprompt $ret-sp
                                           (local.get $prompt))))
           (local.set $args
                      (call $collect-rest-args (local.get $nargs)
                            (local.get $arg0)
                            (local.get $arg1)
                            (local.get $arg2)
                            (i32.const 1)))
           (call $restore-raw-stack
                 (struct.get $cont $raw-stack (local.get $cont)))
           (call $restore-scm-stack
                 (struct.get $cont $scm-stack (local.get $cont)))
           (call $restore-ret-stack
                 (struct.get $cont $ret-stack (local.get $cont)))
           ;; Dyn stack is restored incrementally via $rewind.
           (return_call $rewind
                        (local.get $raw-sp-adjust)
                        (local.get $scm-sp-adjust)
                        (local.get $ret-sp-adjust)
                        (struct.get $cont $dyn-stack (local.get $cont))
                        (i32.const 0)
                        (local.get $args)))

     (func $capture-raw-stack (param $base-sp i32)
           (result (ref $raw-bytevector))
           (local $v (ref $raw-bytevector))
           (local $i i32)
           (local $len i32)
           (local.set $len (i32.sub (global.get $raw-sp) (local.get $base-sp)))
           (local.set $v (array.new_default $raw-bytevector
                                            (local.get $len)))
           (local.set $i (i32.const 0))
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (array.set $raw-bytevector
                             (local.get $v)
                             (local.get $i)
                             (i32.load8_u $raw-stack
                                          (i32.add (local.get $base-sp)
                                                   (local.get $i))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp))))
           (local.get $v))

     (func $capture-scm-stack (param $base-sp i32)
           (result (ref $raw-scmvector))
           (local $v (ref $raw-scmvector))
           (local $i i32)
           (local $len i32)
           (local.set $len (i32.sub (global.get $scm-sp) (local.get $base-sp)))
           (local.set $v
                      (array.new $raw-scmvector
                                 (ref.i31 (i32.const 1))
                                 (local.get $len)))
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (array.set $raw-scmvector
                             (local.get $v)
                             (local.get $i)
                             (ref.as_non_null
                              (table.get $scm-stack
                                         (i32.add (local.get $base-sp)
                                                  (local.get $i)))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp))))
           (local.get $v))

     (func $capture-ret-stack (param $base-sp i32)
           (result (ref $raw-retvector))
           (local $v (ref $raw-retvector))
           (local $i i32)
           (local $len i32)
           (local.set $len (i32.sub (global.get $ret-sp) (local.get $base-sp)))
           (local.set $v
                      (array.new $raw-retvector
                                 (ref.func $invalid-continuation)
                                 (local.get $len)))
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (array.set $raw-retvector
                             (local.get $v)
                             (local.get $i)
                             (ref.as_non_null
                              (table.get $ret-stack
                                         (i32.add (local.get $base-sp)
                                                  (local.get $i)))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp))))
           (local.get $v))

     (func $capture-dyn-stack (param $base-sp i32)
           (result (ref $raw-dynvector))
           (local $v (ref $raw-dynvector))
           (local $i i32)
           (local $len i32)
           (local.set $len (i32.sub (global.get $dyn-sp) (local.get $base-sp)))
           (local.set $v
                      (array.new $raw-dynvector
                                 (struct.new $dyn)
                                 (local.get $len)))
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (array.set $raw-dynvector
                             (local.get $v)
                             (local.get $i)
                             (ref.as_non_null
                              (table.get $dyn-stack
                                         (i32.add (local.get $base-sp)
                                                  (local.get $i)))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp))))
           (local.get $v))

     (func $capture-continuation (param $prompt (ref $dynprompt))
           (param $prompt-dyn-sp i32)
           (result (ref eq))
           (if (result (ref eq))
               (struct.get_u $dynprompt $unwind-only? (local.get $prompt))
               (then (ref.i31 (i32.const 1)))
               (else
                (struct.new
                 $cont
                 (i32.const 0)
                 (ref.func $compose-continuation)
                 (local.get $prompt)
                 (call $capture-raw-stack
                       (struct.get $dynprompt $raw-sp (local.get $prompt)))
                 (call $capture-scm-stack
                       (struct.get $dynprompt $scm-sp (local.get $prompt)))
                 (call $capture-ret-stack
                       ;; Increment to avoid including the prompt unwind
                       ;; continuation.  We rely on the compiler
                       ;; generating code for non-unwind-only prompt
                       ;; bodies that consists of just a closure call.
                       (i32.add
                        (struct.get $dynprompt $ret-sp (local.get $prompt))
                        (i32.const 1)))
                 (call $capture-dyn-stack
                       ;; Incremented to avoid including the prompt
                       ;; itself.
                       (i32.add (local.get $prompt-dyn-sp) (i32.const 1)))))))

     (func $keep-unwinding (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (local $tag (ref eq))
           (local $cont (ref eq))
           (local $args (ref eq))
           (local.set $tag
                      (ref.as_non_null
                       (table.get $scm-stack
                                  (i32.sub (global.get $scm-sp) (i32.const 3)))))
           (local.set $cont
                      (ref.as_non_null
                       (table.get $scm-stack
                                  (i32.sub (global.get $scm-sp) (i32.const 2)))))
           (local.set $args
                      (ref.as_non_null
                       (table.get $scm-stack
                                  (i32.sub (global.get $scm-sp) (i32.const 1)))))
           (global.set $scm-sp (i32.sub (global.get $scm-sp) (i32.const 3)))
           (return_call $unwind-to-prompt
                        (local.get $tag) (local.get $cont) (local.get $args)))

     (func $keep-rewinding (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (local $raw-sp-adjust i32)
           (local $scm-sp-adjust i32)
           (local $ret-sp-adjust i32)
           (local $i i32)
           (local $dyn (ref $raw-dynvector))
           (local $args (ref eq))
           (global.set $raw-sp (i32.sub (global.get $raw-sp) (i32.const 16)))
           (local.set $raw-sp-adjust
                      (i32.load $raw-stack offset=0 (global.get $raw-sp)))
           (local.set $scm-sp-adjust
                      (i32.load $raw-stack offset=4 (global.get $raw-sp)))
           (local.set $ret-sp-adjust
                      (i32.load $raw-stack offset=8 (global.get $raw-sp)))
           (local.set $i
                      (i32.load $raw-stack offset=12 (global.get $raw-sp)))
           (global.set $scm-sp (i32.sub (global.get $scm-sp) (i32.const 2)))
           (local.set $dyn (ref.cast
                            $raw-dynvector
                            (table.get $scm-stack (global.get $scm-sp))))
           (local.set $args (ref.as_non_null
                             (table.get $scm-stack
                                        (i32.add (global.get $scm-sp)
                                                 (i32.const 1)))))
           (return_call $rewind
                        (local.get $raw-sp-adjust)
                        (local.get $scm-sp-adjust)
                        (local.get $ret-sp-adjust)
                        (local.get $dyn)
                        (local.get $i)
                        (local.get $args)))

     (func $unwind-to-prompt
           (param $tag (ref eq)) (param $cont (ref eq)) (param $args (ref eq))
           (local $prompt (ref $dynprompt))
           (local $dynwind (ref $dynwind))
           (local $dyn (ref $dyn))
           ;; During an abort-to-prompt that crosses a dynamic-wind,
           ;; after the dynamic-wind unwinder returns, it could be that
           ;; the dynamic stack is different from where the
           ;; abort-to-prompt started.  It could be that the prompt is
           ;; no longer in the continuation; that's why we look it up
           ;; again here.  More annoyingly, it could be that the prompt
           ;; becomes not unwind-only!  FIXME to check that if $cont is
           ;; #f, that the prompt is indeed still unwind-only.
           (call $find-prompt (local.get $tag))
           (drop) ;; prompt dyn-sp
           (local.set $prompt)
           (loop $lp
             (global.set $dyn-sp
                         (i32.sub (global.get $dyn-sp) (i32.const 1)))
             (local.set $dyn (ref.as_non_null
                              (table.get $dyn-stack (global.get $dyn-sp))))
             (if (ref.eq (local.get $dyn) (local.get $prompt))
                 (then
                  ;; Unwind control stacks.
                  (global.set $raw-sp (struct.get $dynprompt $raw-sp
                                                  (local.get $prompt)))
                  (global.set $scm-sp (struct.get $dynprompt $scm-sp
                                                  (local.get $prompt)))
                  (global.set $ret-sp (struct.get $dynprompt $ret-sp
                                                  (local.get $prompt)))
                  ;; Use apply + values to pass values to handler.
                  (global.set $ret-sp
                              (i32.add (global.get $ret-sp) (i32.const 1)))
                  (call $maybe-grow-ret-stack)
                  (table.set $ret-stack
                             (i32.sub (global.get $ret-sp) (i32.const 1))
                             (struct.get $dynprompt $handler
                                         (local.get $prompt)))
                  (return_call $apply (i32.const 3)
                               (global.get $apply-primitive)
                               (global.get $values-primitive)
                               (struct.new $pair (i32.const 0)
                                           (local.get $cont)
                                           (local.get $args)))))
             ;; Something else is on the stack; what is it?
             (if (ref.test $dynwind (local.get $dyn))
                 (then
                  (local.set $dynwind (ref.cast $dynwind (local.get $dyn)))
                  (global.set $scm-sp (i32.add (global.get $scm-sp) (i32.const 3)))
                  (global.set $ret-sp (i32.add (global.get $ret-sp) (i32.const 1)))
                  (call $maybe-grow-scm-stack)
                  (call $maybe-grow-ret-stack)
                  (table.set $scm-stack
                             (i32.sub (global.get $scm-sp) (i32.const 3))
                             (local.get $tag))
                  (table.set $scm-stack
                             (i32.sub (global.get $scm-sp) (i32.const 2))
                             (local.get $cont))
                  (table.set $scm-stack
                             (i32.sub (global.get $scm-sp) (i32.const 1))
                             (local.get $args))
                  (table.set $ret-stack
                             (i32.sub (global.get $ret-sp) (i32.const 1))
                             (ref.func $keep-unwinding))
                  (return_call_ref $kvarargs
                                   (i32.const 1)
                                   (struct.get $dynwind $unwind
                                               (local.get $dynwind))
                                   (ref.i31 (i32.const 0))
                                   (ref.i31 (i32.const 0))
                                   (struct.get
                                    $proc $func
                                    (struct.get $dynwind $unwind
                                                (local.get $dynwind))))))
             (br_if $lp (ref.test $dynprompt (local.get $dyn)))
             (if (ref.test $dynfluid (local.get $dyn))
                 (then
                  (call $wind-dynfluid (ref.cast $dynfluid (local.get $dyn)))
                  (br $lp)))
             (if (ref.test $dynstate (local.get $dyn))
                 (then
                  (call $wind-dynstate (ref.cast $dynstate (local.get $dyn)))
                  (br $lp)))
             (unreachable)))

     (func $abort-to-prompt (param $nargs i32) (param $arg0 (ref eq))
           (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (if (i32.lt_u (local.get $nargs) (i32.const 2))
               (then
                (call $wrong-num-args (local.get $nargs) (local.get $arg0)
                      (local.get $arg1) (local.get $arg2))
                (unreachable)))
           ;; $arg0 is the closure, $arg1 is tag, and the values are in
           ;; $arg2 and up, which we collect to a rest list.
           (return_call $unwind-to-prompt (local.get $arg1)
                        (call $capture-continuation
                              (call $find-prompt (local.get $arg1)))
                        (call $collect-rest-args (local.get $nargs)
                              (local.get $arg0)
                              (local.get $arg1)
                              (local.get $arg2)
                              (i32.const 2))))
     (global $abort-to-prompt-primitive (ref eq)
             (struct.new $proc (i32.const 0) (ref.func $abort-to-prompt)))

     (func $arg-ref (param $n i32)
           (result (ref eq))
           (block
            $arg-in-register
            (block
             $n3
             (block
              $n4
              (block
               $n5
               (block
                $n6
                (block
                 $n7
                 (block
                  $nv
                  (br_table $arg-in-register
                            $arg-in-register
                            $arg-in-register
                            $n3
                            $n4
                            $n5
                            $n6
                            $n7
                            $nv
                            (local.get $n)))
                 (return (ref.as_non_null
                          (table.get $argv (i32.sub (local.get $n) (i32.const 8))))))
                (return (global.get $arg7)))
               (return (global.get $arg6)))
              (return (global.get $arg5)))
             (return (global.get $arg4)))
            (return (global.get $arg3)))
           (unreachable))

     (func $collect-apply-args
           (param $nargs i32) (param $arg2 (ref eq))
           (result (ref eq))
           (local $ret (ref eq))
           (if (i32.le_u (local.get $nargs) (i32.const 3))
               (then
                (call $die0 (string.const "bad collect-apply-args call"))
                (unreachable)))
           (local.set $ret
                      (call $arg-ref
                            (local.tee $nargs
                                       (i32.sub (local.get $nargs)
                                                (i32.const 1)))))
           (loop $lp
             (if (i32.lt_u (i32.const 3) (local.get $nargs))
                 (then
                  (local.set $ret
                             (struct.new
                              $pair
                              (i32.const 0)
                              (call $arg-ref
                                    (local.tee $nargs
                                               (i32.sub (local.get $nargs)
                                                        (i32.const 1))))
                              (local.get $ret)))
                  (br $lp))))
           (struct.new $pair
                       (i32.const 0)
                       (local.get $arg2)
                       (local.get $ret)))

     (func $apply-to-non-list (param $tail (ref eq))
           (call $die (string.const "$apply-to-non-list") (local.get $tail))
           (unreachable))
     (func $get-callee-code (param $callee (ref eq)) (result (ref $kvarargs))
           (call $die (string.const "$get-callee-code") (local.get $callee))
           (unreachable))

     (func $apply (param $nargs i32) (param $arg0 (ref eq))
           (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (local $args (ref eq))
           (if (i32.lt_u (local.get $nargs) (i32.const 3))
               (then (return_call $wrong-num-args
                                  (local.get $nargs)
                                  (local.get $arg0)
                                  (local.get $arg1)
                                  (local.get $arg2))))
           (local.set $arg0 (local.get $arg1))
           (local.set $args
                      (if (ref eq)
                          (i32.eq (local.get $nargs) (i32.const 3))
                          (then (local.get $arg2))
                          (else (call $collect-apply-args
                                      (local.get $nargs)
                                      (local.get $arg2)))))
           (if
            (ref.test $pair (local.get $args))
            (then
             (local.set $arg1
                        (struct.get $pair $car
                                    (ref.cast $pair (local.get $args))))
             (if
              (ref.test
               $pair
               (local.tee $args
                          (struct.get $pair $cdr
                                      (ref.cast $pair (local.get $args)))))
              (then
               (local.set $arg2
                          (struct.get $pair $car
                                      (ref.cast $pair (local.get $args))))
               (if
                (ref.test
                 $pair
                 (local.tee $args
                            (struct.get $pair $cdr
                                        (ref.cast $pair (local.get $args)))))
                (then
                 (global.set $arg3
                            (struct.get $pair $car
                                        (ref.cast $pair (local.get $args))))
                 (if
                  (ref.test
                   $pair
                   (local.tee $args
                              (struct.get $pair $cdr
                                          (ref.cast $pair (local.get $args)))))
                  (then
                   (global.set $arg4
                               (struct.get $pair $car
                                           (ref.cast $pair (local.get $args))))
                   (if
                    (ref.test
                     $pair
                     (local.tee $args
                                (struct.get $pair $cdr
                                            (ref.cast $pair (local.get $args)))))
                    (then
                     (global.set $arg5
                                 (struct.get $pair $car
                                             (ref.cast $pair (local.get $args))))
                     (if
                      (ref.test
                       $pair
                       (local.tee $args
                                  (struct.get $pair $cdr
                                              (ref.cast $pair (local.get $args)))))
                      (then
                       (global.set $arg6
                                   (struct.get $pair $car
                                               (ref.cast $pair (local.get $args))))
                       (if
                        (ref.test
                         $pair
                         (local.tee $args
                                    (struct.get $pair $cdr
                                                (ref.cast $pair (local.get $args)))))
                        (then
                         (global.set $arg7
                                     (struct.get $pair $car
                                                 (ref.cast $pair (local.get $args))))
                         (local.set $nargs (i32.const 8))
                         (loop $lp
                           (if
                            (ref.test
                             $pair
                             (local.tee $args
                                        (struct.get $pair $cdr
                                                    (ref.cast $pair (local.get $args)))))
                            (then
                             (table.set $argv
                                        (i32.sub (local.get $nargs) (i32.const 8))
                                        (struct.get $pair $car
                                                 (ref.cast $pair (local.get $args))))
                             (local.set $nargs (i32.add (local.get $nargs) (i32.const 1)))
                             (br $lp)))))
                        (else (local.set $nargs (i32.const 7)))))
                      (else (local.set $nargs (i32.const 6)))))
                    (else (local.set $nargs (i32.const 5)))))
                  (else (local.set $nargs (i32.const 4)))))
                (else (local.set $nargs (i32.const 3)))))
              (else (local.set $nargs (i32.const 2)))))
            (else (local.set $nargs (i32.const 1))))
           (if (i32.eqz (ref.eq (local.get $args) (ref.i31 (i32.const 13))))
               (then (return_call $apply-to-non-list (local.get $args))))
           (return_call_ref $kvarargs
                            (local.get $nargs)
                            (local.get $arg0)
                            (local.get $arg1)
                            (local.get $arg2)
                            (if (ref $kvarargs)
                                (ref.test $proc (local.get $arg0))
                                (then (struct.get $proc $func
                                                  (ref.cast $proc (local.get $arg0))))
                                (else (call $get-callee-code (local.get $arg0))))))
     (global $apply-primitive (ref eq)
             (struct.new $proc (i32.const 0) (ref.func $apply)))

     ;; Helper function for $f64->exact
     (func $decode-f64 (param $frac i64) (param $expt i32) (param $sign i32)
           (result (ref eq))
           (if (i32.eq (local.get $sign) (i32.const 1))
               (then (local.set $frac (i64.mul (local.get $frac) (i64.const -1)))))
           (if (ref eq)
               (i32.lt_s (local.get $expt) (i32.const 0))
               ;; divide $frac by 1/(2**|expt|)
               (then
                (call $div
                      (call $s64->bignum (local.get $frac))
                      (call $lsh
                            (call $i32->fixnum (i32.const 2))
                            (i64.mul (i64.const -1)
                                     (i64.extend_i32_s
                                      (local.get $expt))))))
               ;; multiply $frac by 2**expt
               (else
                (call $mul
                      (call $s64->bignum (local.get $frac))
                      (call $lsh
                            (call $i32->fixnum (i32.const 2))
                            (i64.extend_i32_s (local.get $expt)))))))

     (func $f64->exact (param $x f64) (result (ref eq))
           (local $bits i64)
           (local $raw-frac i64)        ; raw significand
           (local $frac i64)            ; decoded significand
           (local $raw-expt i32)        ; biased exponent
           (local $expt i32)            ; actual exponent
           (local $sign i32)

           ;; Split $X into three parts:
           ;; - the fraction [Knuth] or significand (52 bits with an implicit leading 1 bit),
           ;; - the exponent (usually interpreted with an offset of 1023),
           ;; - and a sign bit.
           ;; Special cases:
           ;; (a) E = 0, F = 0 => (signed) zero;
           ;; (b) E = 0, F /= 0 => subnormal: interpret F as
           ;;     non-normalized with an exponent of -1074;
           ;; (c) E = #x7FF, F = 0 => (signed) infinity;
           ;; (d) E = #x7FF, F /= 0 => NaN.
           ;; Otherwise, $X represents (1+F)*(2**(E-1023)).

           (local.set $bits (i64.reinterpret_f64 (local.get $x)))

           (local.set $raw-frac
                      (i64.and (local.get $bits)
                               (i64.const #xFFFFFFFFFFFFF)))
           (local.set $raw-expt
                      (i32.wrap_i64
                       (i64.and (i64.shr_u (local.get $bits) (i64.const 52))
                                (i64.const #x7FF))))
           (local.set $sign (i32.wrap_i64
                             (i64.shr_u (local.get $bits) (i64.const 63))))

           (if (ref eq)
               (i32.and (i32.eqz (local.get $expt))
                        (i64.eqz (local.get $frac)))
               (then                    ; zero (E = 0, F = 0)
                (call $i32->fixnum (i32.const 0)))
               (else
                (if (ref eq)
                    (i32.eqz (local.get $expt))
                    (then               ; subnormal (E = 0, F /= 0)
                     (local.set $frac (local.get $raw-frac))
                     (local.set $expt (i32.const -1074))
                     (return (call $decode-f64
                                   (local.get $frac)
                                   (local.get $expt)
                                   (local.get $sign))))
                    (else
                     (if (ref eq)
                         (i32.eqz (i32.eq (local.get $expt)
                                          (i32.const #x7FF)))
                         (then          ; normal (E /= 0, F /= #xFF)
                          (local.set $frac
                                     (i64.or (local.get $raw-frac)
                                             (i64.const #x10000000000000)))
                          (local.set $expt
                                     (i32.sub (local.get $expt) (i32.const 1023)))
                          (return (call $decode-f64
                                        (local.get $frac)
                                        (local.get $expt)
                                        (local.get $sign))))
                         (else          ; non-finite (inf or NaN)
                          (call $die
                                (string.const "$decode-float bad arg")
                                (struct.new $flonum
                                            (i32.const 0)
                                            (local.get $x)))
                          (unreachable))))))))

     (func $slow-< (param $a (ref eq)) (param $b (ref eq)) (result i32)
           ,(arith-cond 'i32
             `((call $fixnum? (local.get $a))
               ,(arith-cond 'i32
                 `((call $fixnum? (local.get $b))
                   (i32.lt_s (i31.get_s (ref.cast i31 (local.get $a)))
                             (i31.get_s (ref.cast i31 (local.get $b)))))
                 `((ref.test $bignum (local.get $b))
                   (call $lt-fix-big
                         (call $fixnum->i32 (ref.cast i31 (local.get $a)))
                         (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))
                 `((ref.test $flonum (local.get $b))
                   (f64.lt (call $fixnum->f64 (ref.cast i31 (local.get $a)))
                           (call $flonum->f64 (ref.cast $flonum (local.get $b)))))
                 `((ref.test $fraction (local.get $b))
                   (call $slow-<
                         (call $mul
                               (local.get $a)
                               (struct.get $fraction $denom
                                           (ref.cast $fraction (local.get $b))))
                         (struct.get $fraction $num
                                     (ref.cast $fraction (local.get $b)))))
                 '(else
                   (call $die0 (string.const "$slow-<"))
                   (unreachable))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond 'i32
                 `((call $fixnum? (local.get $b))
                   (call $lt-big-fix
                         (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                         (call $fixnum->i32 (ref.cast i31 (local.get $b)))))
                 `((ref.test $bignum (local.get $b))
                   (call $lt-big-big
                         (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                         (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))
                 `((ref.test $flonum (local.get $b))
                   (call $lt-big-flo
                         (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                         (struct.get $flonum $val (ref.cast $flonum (local.get $b)))))
                 `((ref.test $fraction (local.get $b))
                   (call $slow-<
                         (call $mul
                               (local.get $a)
                               (struct.get $fraction $denom
                                           (ref.cast $fraction (local.get $b))))
                         (struct.get $fraction $num
                                     (ref.cast $fraction (local.get $b)))))
                 '(else
                   (call $die0 (string.const "$slow-<"))
                   (unreachable))))
             `((ref.test $flonum (local.get $a))
               ,(arith-cond 'i32
                 `((call $fixnum? (local.get $b))
                   (f64.lt (call $fixnum->f64 (ref.cast i31 (local.get $a)))
                           (struct.get $flonum $val (ref.cast $flonum (local.get $b)))))
                 `((ref.test $bignum (local.get $b))
                   (call $lt-flo-big
                         (struct.get $flonum $val (ref.cast $flonum (local.get $a)))
                         (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))
                 `((ref.test $flonum (local.get $b))
                   (f64.lt (struct.get $flonum $val (ref.cast $flonum (local.get $a)))
                           (struct.get $flonum $val (ref.cast $flonum (local.get $b)))))
                 `((ref.test $fraction (local.get $b))
                   ,(arith-cond
                     'i32
                     '((call $f64-is-nan
                             (call $flonum->f64
                                   (ref.cast $flonum (local.get $a))))
                       (i32.const 0))
                     '((call $f64-is-infinite
                             (call $flonum->f64
                                   (ref.cast $flonum (local.get $a))))
                       (f64.lt (call $flonum->f64
                                     (ref.cast $flonum (local.get $a)))
                               (f64.const 0)))
                     '(else
                       (call $slow-<
                             (call $f64->exact (call $flonum->f64 (ref.cast $flonum (local.get $a))))
                             (local.get $b)))))
                 '(else
                   (call $die0 (string.const "$slow-<"))
                   (unreachable))))
             `((ref.test $fraction (local.get $a))
               ,(arith-cond 'i32
                 `((i32.or (call $fixnum? (local.get $b))
                           (i32.or (ref.test $bignum (local.get $b))
                                   (ref.test $fraction (local.get $b))))
                   (call $slow-<
                         (struct.get $fraction $num
                                     (ref.cast $fraction (local.get $a)))
                         (call $mul
                               (local.get $b)
                               (struct.get $fraction $denom
                                           (ref.cast $fraction (local.get $a))))))
                 `((ref.test $flonum (local.get $b))
                   ,(arith-cond
                     'i32
                     '((call $f64-is-nan
                             (call $flonum->f64
                                   (ref.cast $flonum (local.get $b))))
                       (i32.const 0))
                     '((call $f64-is-infinite
                             (call $flonum->f64
                                   (ref.cast $flonum (local.get $b))))
                       (f64.lt (f64.const 0)
                               (call $flonum->f64
                                     (ref.cast $flonum (local.get $b)))))
                     '(else
                       (call $slow-<
                             (local.get $a)
                             (call $f64->exact
                                   (call $flonum->f64
                                         (ref.cast $flonum (local.get $b))))))))
                 '(else
                   (call $die0 (string.const "$slow-<"))
                   (unreachable))))
             '(else
               (call $die0 (string.const "$slow-<"))
               (unreachable))))

     (func $slow-<= (param $a (ref eq)) (param $b (ref eq)) (result i32)
           ,(arith-cond 'i32
             `((call $fixnum? (local.get $a))
               ,(arith-cond 'i32
                 `((call $fixnum? (local.get $b))
                   (i32.le_s (i31.get_s (ref.cast i31 (local.get $a)))
                             (i31.get_s (ref.cast i31 (local.get $b)))))
                 `((ref.test $bignum (local.get $b))
                   (call $le-fix-big
                         (call $fixnum->i32 (ref.cast i31 (local.get $a)))
                         (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))
                 `((ref.test $flonum (local.get $b))
                   (f64.le (call $fixnum->f64 (ref.cast i31 (local.get $a)))
                           (call $flonum->f64 (ref.cast $flonum (local.get $b)))))
                 `((ref.test $fraction (local.get $b))
                   (call $slow-<=
                         (call $mul
                               (local.get $a)
                               (struct.get $fraction $denom
                                           (ref.cast $fraction (local.get $b))))
                         (struct.get $fraction $num
                                     (ref.cast $fraction (local.get $b)))))
                 '(else
                   (call $die0 (string.const "$slow-<="))
                   (unreachable))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond 'i32
                 `((call $fixnum? (local.get $b))
                   (call $le-big-fix
                         (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                         (call $fixnum->i32 (ref.cast i31 (local.get $b)))))
                 `((ref.test $bignum (local.get $b))
                   (call $le-big-big
                         (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                         (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))
                 `((ref.test $flonum (local.get $b))
                   (call $le-big-flo
                         (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                         (struct.get $flonum $val (ref.cast $flonum (local.get $b)))))
                 `((ref.test $fraction (local.get $b))
                   (call $slow-<=
                         (call $mul
                               (local.get $a)
                               (struct.get $fraction $denom
                                           (ref.cast $fraction (local.get $b))))
                         (struct.get $fraction $num
                                     (ref.cast $fraction (local.get $b)))))
                 '(else
                   (call $die0 (string.const "$slow-<="))
                   (unreachable))))
             `((ref.test $flonum (local.get $a))
               ,(arith-cond 'i32
                 `((call $fixnum? (local.get $b))
                   (i32.le_s (i31.get_s (ref.cast i31 (local.get $a)))
                             (i31.get_s (ref.cast i31 (local.get $a)))))
                 `((ref.test $bignum (local.get $b))
                   (call $le-flo-big
                         (struct.get $flonum $val (ref.cast $flonum (local.get $a)))
                         (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))
                 `((ref.test $flonum (local.get $b))
                   (f64.le (struct.get $flonum $val (ref.cast $flonum (local.get $a)))
                           (struct.get $flonum $val (ref.cast $flonum (local.get $b)))))
                 `((ref.test $fraction (local.get $b))
                   ,(arith-cond
                     'i32
                     '((call $f64-is-nan
                             (call $flonum->f64
                                   (ref.cast $flonum (local.get $a))))
                       (i32.const 0))
                     '((call $f64-is-infinite
                             (call $flonum->f64
                                   (ref.cast $flonum (local.get $a))))
                       (f64.lt (call $flonum->f64
                                     (ref.cast $flonum (local.get $a)))
                               (f64.const 0)))
                     '(else
                       (call $slow-<=
                             (call $f64->exact (call $flonum->f64 (ref.cast $flonum (local.get $a))))
                             (local.get $b)))))
                 '(else
                   (call $die0 (string.const "$slow-<="))
                   (unreachable))))
             `((ref.test $fraction (local.get $a))
               ,(arith-cond 'i32
                 `((i32.or (call $fixnum? (local.get $b))
                           (i32.or (ref.test $bignum (local.get $b))
                                   (ref.test $fraction (local.get $b))))
                   (call $slow-<=
                         (struct.get $fraction $num
                                     (ref.cast $fraction (local.get $a)))
                         (call $mul
                               (local.get $b)
                               (struct.get $fraction $denom
                                           (ref.cast $fraction (local.get $a))))))
                 `((ref.test $flonum (local.get $b))
                   ,(arith-cond
                     'i32
                     '((call $f64-is-nan
                             (call $flonum->f64
                                   (ref.cast $flonum (local.get $b))))
                       (i32.const 0))
                     '((call $f64-is-infinite
                             (call $flonum->f64
                                   (ref.cast $flonum (local.get $b))))
                       (f64.le (f64.const 0)
                               (call $flonum->f64
                                     (ref.cast $flonum (local.get $b)))))
                     '(else
                       (call $slow-<=
                             (local.get $a)
                             (call $f64->exact (call $flonum->f64 (ref.cast $flonum (local.get $b))))))))
                 '(else
                   (call $die0 (string.const "$slow-<="))
                   (unreachable))))
             '(else
               (call $die0 (string.const "$slow-<="))
               (unreachable))))

     (func $slow-= (param $a (ref eq)) (param $b (ref eq)) (result i32)
           ,(arith-cond 'i32
             `((call $fixnum? (local.get $a))
               ,(arith-cond 'i32
                 `((call $fixnum? (local.get $b))
                   (i32.eq (i31.get_s (ref.cast i31 (local.get $a)))
                           (i31.get_s (ref.cast i31 (local.get $b)))))
                 `((ref.test $bignum (local.get $b))
                   (call $eq-fix-big
                         (call $fixnum->i32 (ref.cast i31 (local.get $a)))
                         (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))
                 `((ref.test $flonum (local.get $b))
                   (f64.eq (call $fixnum->f64 (ref.cast i31 (local.get $a)))
                           (call $flonum->f64 (ref.cast $flonum (local.get $b)))))
                 `((ref.test $fraction (local.get $b))
                   (call $slow-=
                         (call $mul
                               (local.get $a)
                               (struct.get $fraction $denom
                                           (ref.cast $fraction (local.get $b))))
                         (struct.get $fraction $num
                                     (ref.cast $fraction (local.get $b)))))
                 '(else
                   (call $die0 (string.const "$slow-="))
                   (unreachable))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond 'i32
                 `((call $fixnum? (local.get $b))
                   (call $eq-big-fix
                         (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                         (call $fixnum->i32 (ref.cast i31 (local.get $b)))))
                 `((ref.test $bignum (local.get $b))
                   (call $eq-big-big
                         (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                         (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))
                 `((ref.test $flonum (local.get $b))
                   (call $eq-big-flo
                         (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                         (struct.get $flonum $val (ref.cast $flonum (local.get $b)))))
                 `((ref.test $fraction (local.get $b))
                   (call $slow-=
                         (call $mul
                               (local.get $a)
                               (struct.get $fraction $denom
                                           (ref.cast $fraction (local.get $b))))
                         (struct.get $fraction $num
                                     (ref.cast $fraction (local.get $b)))))
                 '(else
                   (call $die0 (string.const "$slow-="))
                   (unreachable))))
             `((ref.test $flonum (local.get $a))
               ,(arith-cond 'i32
                 `((call $fixnum? (local.get $b))
                   (i32.eq (i31.get_s (ref.cast i31 (local.get $a)))
                           (i31.get_s (ref.cast i31 (local.get $a)))))
                 `((ref.test $bignum (local.get $b))
                   (call $eq-flo-big
                         (struct.get $flonum $val (ref.cast $flonum (local.get $a)))
                         (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))
                 `((ref.test $flonum (local.get $b))
                   (f64.eq (struct.get $flonum $val (ref.cast $flonum (local.get $a)))
                           (struct.get $flonum $val (ref.cast $flonum (local.get $b)))))
                 `((ref.test $fraction (local.get $b))
                   ,(arith-cond
                     'i32
                     '((call $f64-is-nan
                             (call $flonum->f64
                                   (ref.cast $flonum (local.get $a))))
                       (i32.const 0))
                     '((call $f64-is-infinite
                             (call $flonum->f64
                                   (ref.cast $flonum (local.get $a))))
                       (f64.eq (call $flonum->f64
                                     (ref.cast $flonum (local.get $a)))
                               (f64.const 0)))
                     '(else
                       (call $slow-=
                             (call $f64->exact (call $flonum->f64 (ref.cast $flonum (local.get $a))))
                             (local.get $b)))))
                 '(else
                   (call $die0 (string.const "$slow-="))
                   (unreachable))))
             `((ref.test $fraction (local.get $a))
               ,(arith-cond 'i32
                 `((i32.or (call $fixnum? (local.get $b))
                           (i32.or (ref.test $bignum (local.get $b))
                                   (ref.test $fraction (local.get $b))))
                   (call $slow-=
                         (struct.get $fraction $num
                                     (ref.cast $fraction (local.get $a)))
                         (call $mul
                               (local.get $b)
                               (struct.get $fraction $denom
                                           (ref.cast $fraction (local.get $a))))))
                 `((ref.test $flonum (local.get $b))
                   ,(arith-cond
                     'i32
                     '((call $f64-is-nan
                             (call $flonum->f64
                                   (ref.cast $flonum (local.get $b))))
                       (i32.const 0))
                     '((call $f64-is-infinite
                             (call $flonum->f64
                                   (ref.cast $flonum (local.get $b))))
                       (f64.eq (f64.const 0)
                               (call $flonum->f64
                                     (ref.cast $flonum (local.get $b)))))
                     '(else
                       (call $slow-=
                             (local.get $a)
                             (call $f64->exact (call $flonum->f64 (ref.cast $flonum (local.get $b))))))))
                 '(else
                   (call $die0 (string.const "$slow-="))
                   (unreachable))))
             '(else
               (call $die0 (string.const "$slow-="))
               (unreachable))))

     (func $heap-numbers-equal? (param $a (ref eq)) (param $b (ref eq))
           (result i32)
           ,(arith-cond
             'i32
             `((ref.test $bignum (local.get $a))
               ,(arith-cond
                 'i32
                 `((ref.test $bignum (local.get $b))
                   (call $eq-big-big
                         (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                         (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))
                 '(else
                   (i32.const 0))))
             `((ref.test $flonum (local.get $a))
               ,(arith-cond
                 'i32
                 `((ref.test $flonum (local.get $b))
                   (f64.eq (struct.get $flonum $val (ref.cast $flonum (local.get $a)))
                           (struct.get $flonum $val (ref.cast $flonum (local.get $b)))))
                 '(else
                   (i32.const 0))))
             `((ref.test $fraction (local.get $a))
               ,(arith-cond
                 'i32
                 `((ref.test $fraction (local.get $b))
                   (i32.and
                    (call $slow-=
                          (struct.get $fraction $num
                                      (ref.cast $fraction (local.get $a)))
                          (struct.get $fraction $num
                                      (ref.cast $fraction (local.get $b))))
                    (call $slow-=
                          (struct.get $fraction $denom
                                      (ref.cast $fraction (local.get $a)))
                          (struct.get $fraction $denom
                                      (ref.cast $fraction (local.get $b))))))
                 '(else
                   (i32.const 0))))))

     (func $string-set! (param $str (ref $string)) (param $idx i32)
           (param $ch i32)
           (call $die0 (string.const "$string-set!")) (unreachable))

     ;; cf. compile-test in (hoot compile)
     (func $fixnum? (param $a (ref eq)) (result i32)
           (if (result i32)
               (ref.test i31 (local.get $a))
               (then (i32.eqz
                      (i32.and (i31.get_s (ref.cast i31 (local.get $a)))
                               (i32.const #b1))))
               (else (i32.const 0))))

     (func $fixnum->i32 (param $a (ref i31)) (result i32)
           (i32.shr_s (i31.get_s (local.get $a)) (i32.const 1)))

     (func $fixnum->i64 (param $a (ref i31)) (result i64)
           (i64.extend_i32_s (call $fixnum->i32 (local.get $a))))

     (func $fixnum->f64 (param $a (ref i31)) (result f64)
           (f64.convert_i32_s (call $fixnum->i32 (local.get $a))))

     (func $flonum->f64 (param $a (ref $flonum)) (result f64)
           (struct.get $flonum $val (local.get $a)))

     (func $i32->fixnum (param $a i32) (result (ref i31))
           (ref.i31 (i32.shl (local.get $a) (i32.const 1))))

     (func $i32->bignum (param $a i32) (result (ref eq))
           (struct.new $bignum
                       (i32.const 0)
                       (call $bignum-from-i64
                             (i64.extend_i32_s (local.get $a)))))

     (func $bignum->f64 (param $a (ref $bignum)) (result f64)
           (call $bignum-to-f64 (struct.get $bignum $val (local.get $a))))

     (func $scm->f64 (param $a (ref eq)) (result f64)
           ,(arith-cond 'f64
             '((call $fixnum? (local.get $a))
               (call $fixnum->f64 (ref.cast i31 (local.get $a))))
             '((ref.test $bignum (local.get $a))
               (call $bignum->f64 (ref.cast $bignum (local.get $a))))
             '((ref.test $flonum (local.get $a))
               (struct.get $flonum $val (ref.cast $flonum (local.get $a))))
             '((ref.test $fraction (local.get $a))
               (struct.get
                $flonum $val
                (ref.cast
                 $flonum
                 (call $div
                       (call $inexact
                             (struct.get $fraction $num
                                         (ref.cast $fraction
                                                   (local.get $a))))
                       (call $inexact
                             (struct.get $fraction $num
                                         (ref.cast $fraction
                                                   (local.get $a))))))))))

     (func $numeric-eqv? (param $a (ref eq)) (param $b (ref eq)) (result i32)
           ,(arith-cond 'i32
             `((call $fixnum? (local.get $a))
               ,(arith-cond 'i32
                 '((call $fixnum? (local.get $b))
                   (i32.eq (i31.get_s (ref.cast i31 (local.get $a)))
                           (i31.get_s (ref.cast i31 (local.get $b)))))
                 '((ref.test $bignum (local.get $b))
                   (i32.const 0))
                 '((ref.test $flonum (local.get $b))
                   (i32.const 0))
                 '((ref.test $fraction (local.get $b))
                   (i32.const 0))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond 'i32
                 '((call $fixnum? (local.get $b))
                   (i32.const 0))
                 '((ref.test $bignum (local.get $b))
                   (call $eq-big-big
                         (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                         (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))
                 '((ref.test $flonum (local.get $b))
                   (i32.const 0))
                 '((ref.test $fraction (local.get $b))
                   (i32.const 0))))
             `((ref.test $flonum (local.get $a))
               ,(arith-cond 'i32
                 '((call $fixnum? (local.get $b))
                   (i32.const 0))
                 '((ref.test $bignum (local.get $b))
                   (i32.const 0))
                 '((ref.test $flonum (local.get $b))
                   (f64.eq (struct.get $flonum $val (ref.cast $flonum (local.get $a)))
                           (struct.get $flonum $val (ref.cast $flonum (local.get $b)))))
                 '((ref.test $fraction (local.get $b))
                   (i32.const 0))))
             `((ref.test $fraction (local.get $a))
               ,(arith-cond 'i32
                 '((call $fixnum? (local.get $b))
                   (i32.const 0))
                 '((ref.test $bignum (local.get $b))
                   (i32.const 0))
                 '((ref.test $flonum (local.get $b))
                   (i32.const 0))
                 '((ref.test $fraction (local.get $b))
                   (i32.and (call $numeric-eqv?
                                  (struct.get $fraction $num (ref.cast $fraction (local.get $a)))
                                  (struct.get $fraction $num (ref.cast $fraction (local.get $b))))
                            (call $numeric-eqv?
                                  (struct.get $fraction $denom (ref.cast $fraction (local.get $a)))
                                  (struct.get $fraction $denom (ref.cast $fraction (local.get $b))))))))))

          (func $negative-integer? (param $a (ref eq)) (result i32)
           ,(arith-cond 'i32
             '((call $fixnum? (local.get $a))
               (if (result i32)
                   (i32.ge_s (call $fixnum->i32
                                   (ref.cast i31 (local.get $a)))
                             (i32.const 0))
                   (then (i32.const 0))
                   (else (i32.const 1))))
             `((ref.test $bignum (local.get $a))
               (if (result i32)
                   (f64.ge (call $bignum->f64
                                 (ref.cast $bignum (local.get $a)))
                           (f64.const 0))
                   (then (i32.const 0))
                   (else (i32.const 1))))))

     ;; TODO: write tests once `fixnum?' or similar is available
     (func $normalize-bignum (param $a (ref $bignum)) (result (ref eq))
           (local $a-val (ref extern))
           (local $a64 i64)
           (local.set $a-val (struct.get $bignum $val (local.get $a)))
           (if (ref eq)
               (call $bignum-is-i64 (local.get $a-val))
               (then (local.set $a64 (call $bignum-get-i64 (local.get $a-val)))
                     (if (ref eq)
                         (i32.and (i64.le_s (i64.const #x-20000000)
                                            (local.get $a64))
                                  (i64.le_s (local.get $a64)
                                            (i64.const #x1FFFFFFF)))
                         (then (ref.i31
                                (i32.shl
                                 (i32.wrap_i64 (local.get $a64))
                                 (i32.const 1))))
                         (else (local.get $a))))
               (else (local.get $a))))

     (func $normalize-fraction (param $a (ref $fraction)) (result (ref eq))
           (if (call $numeric-eqv?
                     (struct.get $fraction $denom (local.get $a))
                     (ref.i31 (i32.const 0)))
               (then (call $die
                           (string.const "division-by-zero")
                           (local.get $a))))
           (if (call $negative-integer? (struct.get $fraction $denom (local.get $a)))
               (then (local.set $a
                                (struct.new $fraction
                                            (i32.const 0)
                                            (call $mul
                                                  (struct.get $fraction $num (local.get $a))
                                                  (call $i32->fixnum (i32.const -1)))
                                            (call $mul
                                                  (struct.get $fraction $denom (local.get $a))
                                                  (call $i32->fixnum (i32.const -1)))))))
           (if (ref eq)
               (call $numeric-eqv?
                     (struct.get $fraction $denom (local.get $a))
                     (ref.i31 (i32.const #b10)))
               (then (struct.get $fraction $num (local.get $a)))
               (else (local.get $a))))

     (func $normalize-fraction/gcd (param $a (ref $fraction)) (result (ref eq))
           (local $d (ref eq))
           (local.set $d (call $gcd
                               (struct.get $fraction $num (local.get $a))
                               (struct.get $fraction $denom (local.get $a))))
           (call $normalize-fraction
                 (struct.new $fraction
                             (i32.const 0)
                             (call $quo (struct.get $fraction $num (local.get $a)) (local.get $d))
                             (call $quo (struct.get $fraction $denom (local.get $a)) (local.get $d)))))

     ;; Greatest common divisor: v. TAOCP II 4.5.2 Algorithm A (modern
     ;; Euclidean algorithm). TODO: use a modernized version of
     ;; Algorithm B
     (func $gcd (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           ,(arith-cond
             `((call $fixnum? (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (call $i32->fixnum
                         (call $gcd-i32
                               (call $fixnum->i32 (ref.cast i31 (local.get $a)))
                               (call $fixnum->i32 (ref.cast i31 (local.get $b))))))
                 '((ref.test $bignum (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $bignum-gcd
                                           (call $bignum-from-i32
                                                 (call $fixnum->i32
                                                       (ref.cast i31 (local.get $a))))
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $bignum-gcd
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                                           (call $bignum-from-i32
                                                 (call $fixnum->i32
                                                       (ref.cast i31 (local.get $b))))))))
                 '((ref.test $bignum (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $bignum-gcd
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))))))))

     (func $gcd-i32 (param $a i32) (param $b i32) (result i32)
           (local $r i32)
           ;; Ensure $a and $b are both positive
           (if (i32.lt_s (local.get $a) (i32.const 0))
               (then (local.set $a (i32.mul (local.get $a) (i32.const -1)))))
           (if (i32.lt_s (local.get $b) (i32.const 0))
               (then (local.set $b (i32.mul (local.get $b) (i32.const -1)))))
           (if (i32.eqz (local.get $a))
               (then (return (local.get $b))))
           (if (i32.eqz (local.get $b))
               (then (return (local.get $a))))
           (block $blk
                  (loop $lp
                        (br_if $blk (i32.eqz (local.get $b)))
                        (local.set $r (i32.rem_u (local.get $a)
                                                 (local.get $b)))
                        (local.set $a (local.get $b))
                        (local.set $b (local.get $r))
                        (br $lp)))
           (return (local.get $a)))

     ;; The $A and $B parameters are 30-bit fixnums, with a zero LSB bit
     ;; as the fixnum tag. We examine the top three bits of the result:
     ;; if they're identical, no overflow has occurred and the result is
     ;; represented as a fixnum; otherwise, the result won't fit into a
     ;; fixnum and must be returned as a bignum.
     (func $fixnum-add (param $a i32) (param $b i32) (result (ref eq))
           (local $c i32)
           (local $d i32)
           (local.set $c (i32.add (local.get $a) (local.get $b)))
           (local.set $d (i32.shr_u (local.get $c) (i32.const 29)))
           (if (result (ref eq))
               (i32.or (i32.eqz (local.get $d))
                       (i32.eq (local.get $d)
                               (i32.const #b111)))
               (then (ref.i31 (local.get $c)))
               (else (call $i32->bignum (i32.shr_s (local.get $c) (i32.const 1))))))
     (func $fixnum-sub (param $a i32) (param $b i32) (result (ref eq))
           (local $c i32)
           (local $d i32)
           (local.set $c (i32.sub (local.get $a) (local.get $b)))
           (local.set $d (i32.shr_u (local.get $c) (i32.const 29)))
           (if (result (ref eq))
               (i32.or (i32.eqz (local.get $d))
                       (i32.eq (local.get $d)
                               (i32.const #b111)))
               (then (ref.i31 (local.get $c)))
               (else (call $i32->bignum (i32.shr_s (local.get $c) (i32.const 1))))))
     (func $fixnum-mul (param $a32 i32) (param $b32 i32) (result (ref eq))
           (local $a i64)
           (local $b i64)
           (local $c i64)
           ;; Shift off one operand's tag bit so that the result is also
           ;; properly tagged.
           (local.set $a (i64.extend_i32_s
                          (i32.shr_s (local.get $a32) (i32.const 1))))
           (local.set $b (i64.extend_i32_s (local.get $b32)))
           (local.set $c (i64.mul (local.get $a) (local.get $b)))
           (if (result (ref eq))
               ;; Return a bignum if the (tagged) result lies outside of
               ;; [2^30-1, 2^30].
               (i32.and (i64.ge_s (local.get $c) (i64.const #x-40000000))
                        (i64.le_s (local.get $c) (i64.const #x03FFFFFFF)))
               (then (ref.i31 (i32.wrap_i64 (local.get $c))))
               (else
                (call $normalize-bignum
                      (struct.new $bignum
                                  (i32.const 0)
                                  (call $bignum-from-i64
                                        (i64.shr_s (local.get $c) (i64.const 1))))))))

     (func $fixnum-add* (param $a (ref i31)) (param $b (ref i31)) (result (ref eq))
           (call $fixnum-add
                 (i31.get_s (local.get $a))
                 (i31.get_s (local.get $b))))

     (func $fixnum-sub* (param $a (ref i31)) (param $b (ref i31)) (result (ref eq))
           (call $fixnum-sub
                 (i31.get_s (local.get $a))
                 (i31.get_s (local.get $b))))

     (func $fixnum-mul* (param $a (ref i31)) (param $b (ref i31)) (result (ref eq))
           (call $fixnum-mul
                 (i31.get_s (local.get $a))
                 (i31.get_s (local.get $b))))

     (func $bignum-add* (param $a (ref $bignum)) (param $b (ref $bignum)) (result (ref $bignum))
           (struct.new
            $bignum
            (i32.const 0)
            (call $bignum-add
                  (struct.get $bignum $val (local.get $a))
                  (struct.get $bignum $val (local.get $b)))))

     (func $bignum-sub* (param $a (ref $bignum)) (param $b (ref $bignum)) (result (ref $bignum))
           (struct.new
            $bignum
            (i32.const 0)
            (call $bignum-sub
                  (struct.get $bignum $val (local.get $a))
                  (struct.get $bignum $val (local.get $b)))))

     (func $bignum-mul* (param $a (ref $bignum)) (param $b (ref $bignum)) (result (ref $bignum))
           (struct.new
            $bignum
            (i32.const 0)
            (call $bignum-mul
                  (struct.get $bignum $val (local.get $a))
                  (struct.get $bignum $val (local.get $b)))))

     (func $bignum-quo* (param $a (ref $bignum)) (param $b (ref $bignum)) (result (ref $bignum))
           (struct.new
            $bignum
            (i32.const 0)
            (call $bignum-quo
                  (struct.get $bignum $val (local.get $a))
                  (struct.get $bignum $val (local.get $b)))))

     (func $bignum-rem* (param $a (ref $bignum)) (param $b (ref $bignum)) (result (ref $bignum))
           (struct.new
            $bignum
            (i32.const 0)
            (call $bignum-rem
                  (struct.get $bignum $val (local.get $a))
                  (struct.get $bignum $val (local.get $b)))))

     (func $bignum-mod* (param $a (ref $bignum)) (param $b (ref $bignum)) (result (ref $bignum))
           (struct.new
            $bignum
            (i32.const 0)
            (call $bignum-mod
                  (struct.get $bignum $val (local.get $a))
                  (struct.get $bignum $val (local.get $b)))))

     ;; Exact fraction arithmetic

     ;; Fraction addition
     (func $add-fracnum-fixnum (param $a (ref $fraction)) (param $b (ref i31)) (result (ref eq))
           (call $add-fracnum-fracnum
                 (local.get $a)
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul (local.get $b) (struct.get $fraction $denom (local.get $a)))
                             (struct.get $fraction $denom (local.get $a)))))

     (func $add-fracnum-bignum (param $a (ref $fraction)) (param $b (ref $bignum)) (result (ref eq))
           (call $add-fracnum-fracnum
                 (local.get $a)
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul (local.get $b) (struct.get $fraction $denom (local.get $a)))
                             (struct.get $fraction $denom (local.get $a)))))

     (func $add-fracnum-fracnum (param $a (ref $fraction)) (param $b (ref $fraction)) (result (ref eq))
           (local $d1 (ref eq))
           (local $d2 (ref eq))
           (local $t (ref eq))
           (local.set $d1 (call $gcd
                                (struct.get $fraction $denom (local.get $a))
                                (struct.get $fraction $denom (local.get $b))))
           (if (result (ref eq))
               (if (result i32)
                   (call $fixnum? (local.get $d1))
                   (then (i32.eq (i31.get_s (ref.cast i31 (local.get $d1)))
                                 (i32.const #b10)))
                   (else (f64.eq (call $bignum->f64 (ref.cast $bignum (local.get $a)))
                                 (f64.const 1))))
               (then
                (call $normalize-fraction
                      (struct.new $fraction
                                  (i32.const 0)
                                  (call $add
                                        (call $mul
                                              (struct.get $fraction $num (local.get $a))
                                              (struct.get $fraction $denom (local.get $b)))
                                        (call $mul
                                              (struct.get $fraction $denom (local.get $a))
                                              (struct.get $fraction $num (local.get $b))))
                                  (call $mul
                                        (struct.get $fraction $denom (local.get $a))
                                        (struct.get $fraction $denom (local.get $b))))))
               (else
                (local.set $t
                           (call $add
                                 (call $mul
                                       (struct.get $fraction $num (local.get $a))
                                       (call $quo
                                             (struct.get $fraction $denom (local.get $b))
                                             (local.get $d1)))
                                 (call $mul
                                       (struct.get $fraction $num (local.get $b))
                                       (call $quo
                                             (struct.get $fraction $denom (local.get $a))
                                             (local.get $d1)))))
                (local.set $d2 (call $gcd (local.get $t) (local.get $d1)))
                (call $normalize-fraction
                      (struct.new $fraction
                                  (i32.const 0)
                                  (call $quo
                                        (local.get $t)
                                        (local.get $d2))
                                  (call $mul
                                        (call $quo
                                              (struct.get $fraction $denom (local.get $a))
                                              (local.get $d1))
                                        (call $quo
                                              (struct.get $fraction $denom (local.get $b))
                                              (local.get $d2))))))))

     ;; Fraction subtraction
     (func $sub-fixnum-fracnum (param $a (ref i31)) (param $b (ref $fraction)) (result (ref eq))
           (call $sub-fracnum-fracnum
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul (local.get $a) (struct.get $fraction $denom (local.get $b)))
                             (struct.get $fraction $denom (local.get $b)))
                 (local.get $b)))

     (func $sub-bignum-fracnum (param $a (ref $bignum)) (param $b (ref $fraction)) (result (ref eq))
           (call $sub-fracnum-fracnum
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul (local.get $a) (struct.get $fraction $denom (local.get $b)))
                             (struct.get $fraction $denom (local.get $b)))
                 (local.get $b)))

     (func $sub-fracnum-fixnum (param $a (ref $fraction)) (param $b (ref i31)) (result (ref eq))
           (call $sub-fracnum-fracnum
                 (local.get $a)
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul (local.get $b) (struct.get $fraction $denom (local.get $a)))
                             (struct.get $fraction $denom (local.get $a)))))

     (func $sub-fracnum-bignum (param $a (ref $fraction)) (param $b (ref $bignum)) (result (ref eq))
           (call $sub-fracnum-fracnum
                 (local.get $a)
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul (local.get $b) (struct.get $fraction $denom (local.get $a)))
                             (struct.get $fraction $denom (local.get $a)))))

     (func $sub-fracnum-fracnum (param $a (ref $fraction)) (param $b (ref $fraction)) (result (ref eq))
           (local $d1 (ref eq))
           (local $d2 (ref eq))
           (local $t (ref eq))
           (local.set $d1 (call $gcd
                                (struct.get $fraction $denom (local.get $a))
                                (struct.get $fraction $denom (local.get $b))))
           (if (result (ref eq))
               ;; FIXME: use generic =
               (if (result i32)
                   (ref.test i31 (local.get $d1))
                   (then (i32.eq (i31.get_s (ref.cast i31 (local.get $d1)))
                                 (i32.const #b10)))
                   (else (i32.const 0)))
               (then
                (call $normalize-fraction
                      (struct.new $fraction
                                  (i32.const 0)
                                  (call $sub
                                        (call $mul
                                              (struct.get $fraction $num (local.get $a))
                                              (struct.get $fraction $denom (local.get $b)))
                                        (call $mul
                                              (struct.get $fraction $denom (local.get $a))
                                              (struct.get $fraction $num (local.get $b))))
                                  (call $mul
                                        (struct.get $fraction $denom (local.get $a))
                                        (struct.get $fraction $denom (local.get $b))))))
               (else
                (local.set $t
                           (call $sub
                                 (call $mul
                                       (struct.get $fraction $num (local.get $a))
                                       (call $quo
                                             (struct.get $fraction $denom (local.get $b))
                                             (local.get $d1)))
                                 (call $mul
                                       (struct.get $fraction $num (local.get $b))
                                       (call $quo
                                             (struct.get $fraction $denom (local.get $a))
                                             (local.get $d1)))))
                (local.set $d2 (call $gcd (local.get $t) (local.get $d1)))
                (call $normalize-fraction
                      (struct.new $fraction
                                  (i32.const 0)
                                  (call $quo
                                        (local.get $t)
                                        (local.get $d2))
                                  (call $mul
                                        (call $quo
                                              (struct.get $fraction $denom (local.get $a))
                                              (local.get $d1))
                                        (call $quo
                                              (struct.get $fraction $denom (local.get $b))
                                              (local.get $d2))))))))

     ;; Fraction multiplication
     (func $mul-fracnum-fixnum (param $a (ref $fraction)) (param $b (ref i31)) (result (ref eq))
           (call $normalize-fraction/gcd
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul (local.get $b) (struct.get $fraction $num (local.get $a)))
                             (struct.get $fraction $denom (local.get $a)))))

     (func $mul-fracnum-bignum (param $a (ref $fraction)) (param $b (ref $bignum)) (result (ref eq))
           (call $normalize-fraction/gcd
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul (local.get $b) (struct.get $fraction $num (local.get $a)))
                             (struct.get $fraction $denom (local.get $a)))))

     (func $mul-fracnum-fracnum (param $a (ref $fraction)) (param $b (ref $fraction)) (result (ref eq))
           (local $d1 (ref eq))
           (local $d2 (ref eq))
           (local.set $d1 (call $gcd
                                (struct.get $fraction $num (local.get $a))
                                (struct.get $fraction $denom (local.get $b))))
           (local.set $d2 (call $gcd
                                (struct.get $fraction $denom (local.get $a))
                                (struct.get $fraction $num (local.get $b))))
           (call $normalize-fraction
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul
                                   (call $quo
                                         (struct.get $fraction $num (local.get $a))
                                         (local.get $d1))
                                   (call $quo
                                         (struct.get $fraction $num (local.get $b))
                                         (local.get $d2)))
                             (call $mul
                                   (call $quo
                                         (struct.get $fraction $denom (local.get $a))
                                         (local.get $d2))
                                   (call $quo
                                         (struct.get $fraction $denom (local.get $b))
                                         (local.get $d1))))))

     ;; Fraction division
     (func $div-fixnum-fracnum (param $a (ref i31)) (param $b (ref $fraction)) (result (ref eq))
           (call $normalize-fraction/gcd
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul (local.get $a) (struct.get $fraction $denom (local.get $b)))
                             (struct.get $fraction $num (local.get $b)))))

     (func $div-bignum-fracnum (param $a (ref $bignum)) (param $b (ref $fraction)) (result (ref eq))
           (call $normalize-fraction/gcd
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul (local.get $a) (struct.get $fraction $denom (local.get $b)))
                             (struct.get $fraction $num (local.get $b)))))

     (func $div-fracnum-fixnum (param $a (ref $fraction)) (param $b (ref i31)) (result (ref eq))
           (call $normalize-fraction/gcd
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul (local.get $b) (struct.get $fraction $denom (local.get $a)))
                             (struct.get $fraction $num (local.get $a)))))

     (func $div-fracnum-bignum (param $a (ref $fraction)) (param $b (ref $bignum)) (result (ref eq))
           (call $normalize-fraction/gcd
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul (local.get $b) (struct.get $fraction $denom (local.get $a)))
                             (struct.get $fraction $num (local.get $a)))))

     (func $div-fracnum-fracnum (param $a (ref $fraction)) (param $b (ref $fraction)) (result (ref eq))
           (call $normalize-fraction/gcd
                 (struct.new $fraction
                             (i32.const 0)
                             (call $mul
                                   (struct.get $fraction $num (local.get $a))
                                   (struct.get $fraction $denom (local.get $b)))
                             (call $mul
                                   (struct.get $fraction $denom (local.get $a))
                                   (struct.get $fraction $num (local.get $b))))))

     (func $add (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           ,(arith-cond
             `((call $fixnum? (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $fixnum-add*
                                 (ref.cast i31 (local.get $a))
                                 (ref.cast i31 (local.get $b)))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-add*
                                       (struct.new $bignum
                                                   (i32.const 0)
                                                   (call $bignum-from-i32
                                                         (i32.shr_s (i31.get_s (ref.cast i31 (local.get $a)))
                                                                    (i32.const 1))))
                                       (ref.cast $bignum (local.get $b))))))
                 '((ref.test $flonum (local.get $b))
                   (return
                    (struct.new $flonum
                                (i32.const 0)
                                (f64.add
                                 (call $fixnum->f64 (ref.cast i31 (local.get $a)))
                                 (call $flonum->f64 (ref.cast $flonum (local.get $b)))))))
                 '((ref.test $fraction (local.get $b))
                   (return (call $add-fracnum-fixnum
                                 (ref.cast $fraction (local.get $b))
                                 (ref.cast i31 (local.get $a)))))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-add*
                                       (ref.cast $bignum (local.get $a))
                                       (struct.new $bignum
                                                   (i32.const 0)
                                                   (call $bignum-from-i32
                                                         (i32.shr_s (i31.get_s (ref.cast i31 (local.get $b)))
                                                                    (i32.const 1))))))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-add*
                                       (ref.cast $bignum (local.get $a))
                                       (ref.cast $bignum (local.get $b))))))
                 '((ref.test $flonum (local.get $b))
                   (return
                    (struct.new $flonum
                                (i32.const 0)
                                (f64.add
                                 (call $bignum->f64 (ref.cast $bignum (local.get $a)))
                                 (call $flonum->f64 (ref.cast $flonum (local.get $b)))))))
                 '((ref.test $fraction (local.get $b))
                   (return (call $add-fracnum-bignum
                                 (ref.cast $fraction (local.get $b))
                                 (ref.cast $bignum (local.get $a)))))))
             `((ref.test $flonum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.add
                                        (call $flonum->f64 (ref.cast $flonum (local.get $a)))
                                        (call $fixnum->f64 (ref.cast i31 (local.get $b)))))))
                 '((ref.test $bignum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.add
                                        (call $flonum->f64 (ref.cast $flonum (local.get $a)))
                                        (call $bignum->f64 (ref.cast $bignum (local.get $b)))))))
                 '((ref.test $flonum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.add
                                        (call $flonum->f64 (ref.cast $flonum (local.get $a)))
                                        (call $flonum->f64 (ref.cast $flonum (local.get $b)))))))))
             `((ref.test $fraction (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $add-fracnum-fixnum
                                 (ref.cast $fraction (local.get $a))
                                 (ref.cast i31 (local.get $b)))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $add-fracnum-bignum
                                 (ref.cast $fraction (local.get $a))
                                 (ref.cast $bignum (local.get $b)))))
                 '((ref.test $fraction (local.get $b))
                   (return (call $add-fracnum-fracnum
                                 (ref.cast $fraction (local.get $a))
                                 (ref.cast $fraction (local.get $b)))))))))

     (func $sub (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           ,(arith-cond
             `((call $fixnum? (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $fixnum-sub*
                                 (ref.cast i31 (local.get $a))
                                 (ref.cast i31 (local.get $b)))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-sub*
                                       (struct.new $bignum
                                                   (i32.const 0)
                                                   (call $bignum-from-i32
                                                         (i32.shr_s (i31.get_s (ref.cast i31 (local.get $a)))
                                                                    (i32.const 1))))
                                       (ref.cast $bignum (local.get $b))))))
                 '((ref.test $flonum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.sub
                                        (call $fixnum->f64 (ref.cast i31 (local.get $a)))
                                        (call $flonum->f64 (ref.cast $flonum (local.get $b)))))))
                 '((ref.test $fraction (local.get $b))
                   (return (call $sub-fixnum-fracnum
                                 (ref.cast i31 (local.get $a))
                                 (ref.cast $fraction (local.get $b)))))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-sub*
                                       (ref.cast $bignum (local.get $a))
                                       (struct.new $bignum
                                                   (i32.const 0)
                                                   (call $bignum-from-i32
                                                         (i32.shr_s (i31.get_s (ref.cast i31 (local.get $b)))
                                                                    (i32.const 1))))))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-sub*
                                       (ref.cast $bignum (local.get $a))
                                       (ref.cast $bignum (local.get $b))))))
                 '((ref.test $flonum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.sub
                                        (call $bignum->f64 (ref.cast $bignum (local.get $a)))
                                        (call $flonum->f64 (ref.cast $flonum (local.get $b)))))))
                 '((ref.test $fraction (local.get $b))
                   (return (call $sub-bignum-fracnum
                                 (ref.cast $bignum (local.get $a))
                                 (ref.cast $fraction (local.get $b)))))))
             `((ref.test $flonum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.sub
                                        (call $flonum->f64 (ref.cast $flonum (local.get $a)))
                                        (call $fixnum->f64 (ref.cast i31 (local.get $b)))))))
                 '((ref.test $bignum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.sub
                                        (call $flonum->f64 (ref.cast $flonum (local.get $a)))
                                        (call $bignum->f64 (ref.cast $bignum (local.get $b)))))))
                 '((ref.test $flonum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.sub
                                        (call $flonum->f64 (ref.cast $flonum (local.get $a)))
                                        (call $flonum->f64 (ref.cast $flonum (local.get $b)))))))))
             `((ref.test $fraction (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $sub-fracnum-fixnum
                                 (ref.cast $fraction (local.get $a))
                                 (ref.cast i31 (local.get $b)))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $sub-fracnum-bignum
                                 (ref.cast $fraction (local.get $a))
                                 (ref.cast $bignum (local.get $b)))))
                 '((ref.test $fraction (local.get $b))
                   (return (call $sub-fracnum-fracnum
                                 (ref.cast $fraction (local.get $a))
                                 (ref.cast $fraction (local.get $b)))))))))

     (func $mul (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           ,(arith-cond
             `((call $fixnum? (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $fixnum-mul*
                                 (ref.cast i31 (local.get $a))
                                 (ref.cast i31 (local.get $b)))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-mul*
                                       (struct.new $bignum
                                                   (i32.const 0)
                                                   (call $bignum-from-i32
                                                         (i32.shr_s (i31.get_s (ref.cast i31 (local.get $a)))
                                                                    (i32.const 1))))
                                       (ref.cast $bignum (local.get $b))))))
                 '((ref.test $flonum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.mul
                                        (call $fixnum->f64 (ref.cast i31 (local.get $a)))
                                        (call $flonum->f64 (ref.cast $flonum (local.get $b)))))))
                 '((ref.test $fraction (local.get $b))
                   (return (call $mul-fracnum-fixnum
                                 (ref.cast $fraction (local.get $b))
                                 (ref.cast i31 (local.get $a)))))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-mul*
                                       (ref.cast $bignum (local.get $a))
                                       (struct.new $bignum
                                                   (i32.const 0)
                                                   (call $bignum-from-i32
                                                         (i32.shr_s (i31.get_s (ref.cast i31 (local.get $b)))
                                                                    (i32.const 1))))))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-mul*
                                       (ref.cast $bignum (local.get $a))
                                       (ref.cast $bignum (local.get $b))))))
                 '((ref.test $flonum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.mul
                                        (call $bignum->f64 (ref.cast $bignum (local.get $a)))
                                        (call $flonum->f64 (ref.cast $flonum (local.get $b)))))))
                 '((ref.test $fraction (local.get $b))
                   (return (call $mul-fracnum-bignum
                                 (ref.cast $fraction (local.get $b))
                                 (ref.cast $bignum (local.get $a)))))))
             `((ref.test $flonum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.mul
                                        (call $flonum->f64 (ref.cast $flonum (local.get $a)))
                                        (call $fixnum->f64 (ref.cast i31 (local.get $b)))))))
                 '((ref.test $bignum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.mul
                                        (call $flonum->f64 (ref.cast $flonum (local.get $a)))
                                        (call $bignum->f64 (ref.cast $bignum (local.get $b)))))))
                 '((ref.test $flonum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.mul
                                        (call $flonum->f64 (ref.cast $flonum (local.get $a)))
                                        (call $flonum->f64 (ref.cast $flonum (local.get $b)))))))))
             `((ref.test $fraction (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $mul-fracnum-fixnum
                                 (ref.cast $fraction (local.get $a))
                                 (ref.cast i31 (local.get $b)))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $mul-fracnum-bignum
                                 (ref.cast $fraction (local.get $a))
                                 (ref.cast $bignum (local.get $b)))))
                 '((ref.test $fraction (local.get $b))
                   (return (call $mul-fracnum-fracnum
                                 (ref.cast $fraction (local.get $a))
                                 (ref.cast $fraction (local.get $b)))))))))

     (func $div (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           ,(arith-cond
             `((call $fixnum? (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $normalize-fraction/gcd
                                 (struct.new $fraction
                                             (i32.const 0)
                                             (local.get $a)
                                             (local.get $b)))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-fraction/gcd
                                 (struct.new $fraction
                                             (i32.const 0)
                                             (local.get $a)
                                             (local.get $b)))))
                 '((ref.test $flonum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.div
                                        (call $fixnum->f64 (ref.cast i31 (local.get $a)))
                                        (call $flonum->f64 (ref.cast $flonum (local.get $b)))))))
                 '((ref.test $fraction (local.get $b))
                   (return (call $div-fixnum-fracnum
                                 (ref.cast i31 (local.get $a))
                                 (ref.cast $fraction (local.get $b)))))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $normalize-fraction/gcd
                                 (struct.new $fraction
                                             (i32.const 0)
                                             (local.get $a)
                                             (local.get $b)))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-fraction/gcd
                                 (struct.new $fraction
                                             (i32.const 0)
                                             (local.get $a)
                                             (local.get $b)))))
                 '((ref.test $flonum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.div
                                        (call $bignum->f64 (ref.cast $bignum (local.get $a)))
                                        (call $flonum->f64 (ref.cast $flonum (local.get $b)))))))
                 '((ref.test $fraction (local.get $b))
                   (return (call $div-fixnum-fracnum
                                 (ref.cast i31 (local.get $a))
                                 (ref.cast $fraction (local.get $b)))))))
             `((ref.test $flonum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.div
                                        (call $flonum->f64 (ref.cast $flonum (local.get $a)))
                                        (call $fixnum->f64 (ref.cast i31 (local.get $b)))))))
                 '((ref.test $bignum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.div
                                        (call $flonum->f64 (ref.cast $flonum (local.get $a)))
                                        (call $bignum->f64 (ref.cast $bignum (local.get $b)))))))
                 '((ref.test $flonum (local.get $b))
                   (return (struct.new $flonum
                                       (i32.const 0)
                                       (f64.div
                                        (call $flonum->f64 (ref.cast $flonum (local.get $a)))
                                        (call $flonum->f64 (ref.cast $flonum (local.get $b)))))))))
             `((ref.test $fraction (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $div-fracnum-fixnum
                                 (ref.cast $fraction (local.get $a))
                                 (ref.cast i31 (local.get $b)))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $div-fracnum-bignum
                                 (ref.cast $fraction (local.get $a))
                                 (ref.cast $bignum (local.get $b)))))
                 '((ref.test $fraction (local.get $b))
                   (return (call $div-fracnum-fracnum
                                 (ref.cast $fraction (local.get $a))
                                 (ref.cast $fraction (local.get $b)))))))))

     (func $quo (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           ,(arith-cond
             `((call $fixnum? (local.get $a))
               ,(arith-cond
                 ;; TODO: implement for b = -1
                 '((call $fixnum? (local.get $b))
                   (i31.get_s (ref.cast i31 (local.get $a))) (i32.const 1) (i32.shr_s)
                   (i31.get_s (ref.cast i31 (local.get $b))) (i32.const 1) (i32.shr_s)
                   (i32.div_s)
                   (i32.const 1) (i32.shl)
                   (ref.i31))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-quo*
                                       (struct.new $bignum
                                                   (i32.const 0)
                                                   (call $bignum-from-i32
                                                         (call $fixnum->i32
                                                               (ref.cast i31 (local.get $a)))))
                                       (ref.cast $bignum (local.get $b))))))
                 ;; TODO: integer flonums
                 '((ref.test $flonum (local.get $b))
                   (call $die0 (string.const "$quo/fixnum-flonum"))
                   (unreachable))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-quo*
                                       (ref.cast $bignum (local.get $a))
                                       (struct.new $bignum
                                                   (i32.const 0)
                                                   (call $bignum-from-i32
                                                         (call $fixnum->i32
                                                               (ref.cast i31 (local.get $b)))))))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-quo*
                                       (ref.cast $bignum (local.get $a))
                                       (ref.cast $bignum (local.get $b))))))
                 ;; TODO: integer flonums
                 '((ref.test $flonum (local.get $b))
                   (call $die0 (string.const "$quo/bignum-flonum"))
                   (unreachable))))
             ;; TODO: integer flonums
             `((ref.test $flonum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (call $die0 (string.const "$quo/flonum-fixnum"))
                   (unreachable))
                 '((ref.test $bignum (local.get $b))
                   (call $die0 (string.const "$quo/flonum-bignum"))
                   (unreachable))
                 '((ref.test $flonum (local.get $b))
                   (call $die0 (string.const "$quo/flonum-flonum"))
                   (unreachable))))))

     (func $rem (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           ,(arith-cond
             `((call $fixnum? (local.get $a))
               ,(arith-cond
                 ;; TODO: signal overflow error ($b = 0)
                 '((call $fixnum? (local.get $b))
                   (call $die0 (string.const "$rem/fixnum-fixnum"))
                   (unreachable))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-rem*
                                       (struct.new $bignum
                                                   (i32.const 0)
                                                   (call $bignum-from-i32
                                                         (call $fixnum->i32
                                                               (ref.cast i31 (local.get $a)))))
                                       (ref.cast $bignum (local.get $b))))))
                 ;; TODO: integer flonums
                 '((ref.test $flonum (local.get $b))
                   (call $die0 (string.const "$rem/fixnum-flonum"))
                   (unreachable))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-rem*
                                       (ref.cast $bignum (local.get $a))
                                       (struct.new $bignum
                                                   (i32.const 0)
                                                   (call $bignum-from-i32
                                                         (call $fixnum->i32
                                                               (ref.cast i31 (local.get $b)))))))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-rem*
                                       (ref.cast $bignum (local.get $a))
                                       (ref.cast $bignum (local.get $b))))))
                 ;; TODO: integer flonums
                 '((ref.test $flonum (local.get $b))
                   (call $die0 (string.const "$rem/bignum-flonum"))
                   (unreachable))))
             ;; TODO: integer flonums
             `((ref.test $flonum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (call $die0 (string.const "$rem/flonum-fixnum"))
                   (unreachable))
                 '((ref.test $bignum (local.get $b))
                   (call $die0 (string.const "$rem/flonum-bignum"))
                   (unreachable))
                 '((ref.test $flonum (local.get $b))
                   (call $die0 (string.const "$rem/flonum-flonum"))
                   (unreachable))))))

     (func $mod (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           ,(arith-cond
             `((call $fixnum? (local.get $a))
               ,(arith-cond
                 ;; TODO: signal overflow error ($b = 0)
                 '((call $fixnum? (local.get $b))
                   (call $die0 (string.const "$mod/fixnum-fixnum"))
                   (unreachable))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-mod*
                                       (struct.new $bignum
                                                   (i32.const 0)
                                                   (call $bignum-from-i32
                                                         (call $fixnum->i32
                                                               (ref.cast i31 (local.get $a)))))
                                       (ref.cast $bignum (local.get $b))))))
                 ;; TODO: integer flonums
                 '((ref.test $flonum (local.get $b))
                   (call $die0 (string.const "$mod/fixnum-flonum"))
                   (unreachable))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-mod*
                                       (ref.cast $bignum (local.get $a))
                                       (struct.new $bignum
                                                   (i32.const 0)
                                                   (call $bignum-from-i32
                                                         (call $fixnum->i32
                                                               (ref.cast i31 (local.get $b)))))))))
                 '((ref.test $bignum (local.get $b))
                   (return (call $normalize-bignum
                                 (call $bignum-mod*
                                       (ref.cast $bignum (local.get $a))
                                       (ref.cast $bignum (local.get $b))))))
                 ;; TODO: integer flonums
                 '((ref.test $flonum (local.get $b))
                   (call $die0 (string.const "$mod/flonum-flonum"))
                   (unreachable))))
             ;; TODO: integer flonums
             `((ref.test $flonum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (call $die0 (string.const "$mod/flonum-fixnum"))
                   (unreachable))
                 '((ref.test $bignum (local.get $b))
                   (call $die0 (string.const "$mod/flonum-bignum"))
                   (unreachable))
                 '((ref.test $flonum (local.get $b))
                                      (call $die0 (string.const "$mod/flonum-flonum"))
                   (unreachable))))))

     ;; Bitwise operators and shifts

     (func $logand (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           ,(arith-cond '(ref eq)
             `((call $fixnum? (local.get $a))
               ,(arith-cond '(ref eq)
                 '((call $fixnum? (local.get $b))
                   (call $i32->fixnum
                         (i32.and (call $fixnum->i32 (ref.cast i31 (local.get $a)))
                                  (call $fixnum->i32 (ref.cast i31 (local.get $b))))))
                 '((ref.test $bignum (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $bignum-logand-i32
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $b)))
                                           (call $fixnum->i32 (ref.cast i31 (local.get $a)))))))
                 '(else
                   (call $die0 (string.const "$logand"))
                   (unreachable))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond '(ref eq)
                 '((call $fixnum? (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $bignum-logand-i32
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                                           (call $fixnum->i32 (ref.cast i31 (local.get $b)))))))
                 '((ref.test $bignum (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $bignum-logand-bignum
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))))
                 `(else
                   (call $die0 (string.const "$logand"))
                   (unreachable))))
             '((ref.test $flonum (local.get $a))
               (call $die0 (string.const "$logand"))
               (unreachable))
             '((ref.test $fraction (local.get $a))
               (call $die0 (string.const "$logand"))
               (unreachable))
             '(else
               (call $die0 (string.const "$logand"))
               (unreachable))))

     (func $logior (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           ,(arith-cond
             `((call $fixnum? (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (call $i32->fixnum
                         (i32.or (call $fixnum->i32 (ref.cast i31 (local.get $a)))
                                 (call $fixnum->i32 (ref.cast i31 (local.get $b))))))
                 '((ref.test $bignum (local.get $b))
                   (call $normalize-bignum
                    (struct.new $bignum
                                (i32.const 0)
                                (call $bignum-logior-i32
                                      (struct.get $bignum $val (ref.cast $bignum (local.get $b)))
                                      (call $fixnum->i32 (ref.cast i31 (local.get $a)))))))
                 '(else
                   (call $die0 (string.const "$logior"))
                   (unreachable))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (call $normalize-bignum
                    (struct.new $bignum
                                (i32.const 0)
                                (call $bignum-logior-i32
                                      (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                                      (call $fixnum->i32 (ref.cast i31 (local.get $b)))))))
                 '((ref.test $bignum (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $bignum-logior-bignum
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))))
                 `(else
                   (call $die0 (string.const "$logior"))
                   (unreachable))))
             '((ref.test $flonum (local.get $a))
               (call $die0 (string.const "$logior"))
               (unreachable))
             '((ref.test $fraction (local.get $a))
               (call $die0 (string.const "$logior"))
               (unreachable))
             '(else
               (call $die0 (string.const "$logior"))
               (unreachable))))

     (func $logxor (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           ,(arith-cond
             `((call $fixnum? (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (call $i32->fixnum
                         (i32.xor (call $fixnum->i32 (ref.cast i31 (local.get $a)))
                                  (call $fixnum->i32 (ref.cast i31 (local.get $b))))))
                 '((ref.test $bignum (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $bignum-logxor-i32
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $b)))
                                           (call $fixnum->i32 (ref.cast i31 (local.get $a)))))))
                 '(else
                   (call $die0 (string.const "$logxor"))
                   (unreachable))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $bignum-logxor-i32
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                                           (call $fixnum->i32 (ref.cast i31 (local.get $b)))))))
                 '((ref.test $bignum (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $bignum-logxor-bignum
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))))
                 `(else
                   (call $die0 (string.const "$logxor"))
                   (unreachable))))
             '((ref.test $flonum (local.get $a))
               (call $die0 (string.const "$logxor"))
               (unreachable))
             '((ref.test $fraction (local.get $a))
               (call $die0 (string.const "$logxor"))
               (unreachable))
             '(else
               (call $die0 (string.const "$logxor"))
               (unreachable))))

     (func $logsub (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           ,(arith-cond
             `((call $fixnum? (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   '(call $i32->fixnum
                          (i32.and
                           (call $fixnum->i32 (ref.cast i31 (local.get $a)))
                           (i32.xor (call $fixnum->i32
                                          (ref.cast i31 (local.get $b)))
                                    (i32.const -1)))))
                 '((ref.test $bignum (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $i32-logsub-bignum
                                           (call $fixnum->i32 (ref.cast i31 (local.get $a)))
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $b)))))))
                 '(else
                   (call $die0 (string.const "$logsub"))
                   (unreachable))))
             `((ref.test $bignum (local.get $a))
               ,(arith-cond
                 '((call $fixnum? (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $bignum-logsub-i32
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                                           (call $fixnum->i32 (ref.cast i31 (local.get $b)))))))
                 '((ref.test $bignum (local.get $b))
                   (call $normalize-bignum
                         (struct.new $bignum
                                     (i32.const 0)
                                     (call $bignum-logsub-bignum
                                           (struct.get $bignum $val (ref.cast $bignum (local.get $a)))
                                           (struct.get $bignum $val (ref.cast i31 (local.get $b)))))))
                 `(else
                   (call $die0 (string.const "$logsub"))
                   (unreachable))))
             '((ref.test $flonum (local.get $a))
               (call $die0 (string.const "$logsub"))
               (unreachable))
             '((ref.test $fraction (local.get $a))
               (call $die0 (string.const "$logsub"))
               (unreachable))
             '(else
               (call $die0 (string.const "$logsub"))
               (unreachable))))

     (func $rsh (param $a (ref eq)) (param $b i64) (result (ref eq))
           ,(arith-cond
             '((ref.test $bignum (local.get $a))
               (call $normalize-bignum
                     (struct.new $bignum
                                 (i32.const 0)
                                 (call $bignum-rsh
                                       (struct.get $bignum $val
                                                   (ref.cast $bignum (local.get $a)))
                                       (local.get $b)))))
             '(else
               (call $die
                     (string.const "$rsh bad first arg")
                     (local.get $a))
               (unreachable))))

     (func $lsh (param $a (ref eq)) (param $b i64) (result (ref eq))
           ,(arith-cond
             '((call $fixnum? (local.get $a))
               (call $normalize-bignum
                     (struct.new $bignum
                                 (i32.const 0)
                                 (call $i32-lsh
                                       (call $fixnum->i32 (ref.cast i31 (local.get $a)))
                                       (local.get $b)))))
             '((ref.test $bignum (local.get $a))
               (struct.new $bignum
                           (i32.const 0)
                           (call $bignum-lsh
                                 (struct.get $bignum $val
                                             (ref.cast $bignum (local.get $a)))
                                 (local.get $b))))
             '(else
               (call $die
                     (string.const "$lsh bad first arg")
                     (local.get $a))
               (unreachable))))

     (func $inexact (param $x (ref eq)) (result (ref $flonum))
           ,(arith-cond '(ref $flonum)
             `((call $fixnum? (local.get $x))
               (struct.new $flonum
                           (i32.const 0)
                           (call $fixnum->f64
                                 (ref.cast i31 (local.get $x)))))
             `((ref.test $bignum (local.get $x))
               (struct.new $flonum
                           (i32.const 0)
                           (call $bignum->f64
                                 (ref.cast $bignum (local.get $x)))))
             `((ref.test $flonum (local.get $x))
               (ref.cast $flonum (local.get $x)))
             ;; FIXME: improve fraction approximation
             `((ref.test $fraction (local.get $x))
               (ref.cast $flonum
                (call $div
                      (call $inexact
                            (struct.get $fraction $num (ref.cast $fraction (local.get $x))))
                      (call $inexact
                            (struct.get $fraction $denom (ref.cast $fraction (local.get $x)))))))))

     ;; compute (logand x #xffffFFFF).  precondition: x is exact integer.
     (func $scm->u32/truncate (param $x (ref eq)) (result i32)
           (if i32
               (ref.test i31 (local.get $x))
               (then (i32.shr_s (i31.get_s (ref.cast i31 (local.get $x)))
                                (i32.const 1)))
               (else
                (i32.wrap_i64
                 (call $bignum-get-i64
                       (struct.get $bignum $val
                                   (ref.cast $bignum (local.get $x))))))))

     (func $abs (param $x (ref eq)) (result (ref eq))
           ,(arith-cond
             '((call $fixnum? (local.get $x))
               (if (result (ref eq))
                   (call $negative-integer? (local.get $x))
                   (then (call $mul (local.get $x) (call $i32->fixnum (i32.const -1))))
                   (else (local.get $x))))
             '((ref.test $bignum (local.get $x))
               (if (result (ref eq))
                   (call $negative-integer? (local.get $x))
                   (then (call $mul (local.get $x) (call $i32->fixnum (i32.const -1))))
                   (else (local.get $x))))
             ;; FIXME: not actually tested yet, as the compiler typically uses $fabs
             '((ref.test $flonum (local.get $x))
               (struct.new $flonum
                           (i32.const 0)
                           (f64.abs (call $flonum->f64 (ref.cast $flonum (local.get $x))))))
             '((ref.test $fraction (local.get $x))
               (if (result (ref eq))
                   (call $negative-integer?
                         (struct.get $fraction $num
                                     (ref.cast $fraction (local.get $x))))
                   (then (call $mul (local.get $x) (call $i32->fixnum (i32.const -1))))
                   (else (local.get $x))))))

     (func $remz (param $m (ref eq)) (param $n (ref eq))
           (result (ref eq))
           ,(arith-cond
             `((call $fixnum? (local.get $m))
               ,(arith-cond
                 '((call $fixnum? (local.get $n))
                   (call $i32->fixnum
                         (i32.rem_s
                          (call $fixnum->i32
                                (ref.cast i31 (local.get $m)))
                          (call $fixnum->i32
                                (ref.cast i31 (local.get $n))))))
                 '((ref.test $bignum (local.get $n))
                   (call $bignum-rem*
                         (ref.cast $bignum
                                   (call $i32->bignum
                                         (call $fixnum->i32
                                               (ref.cast i31
                                                         (local.get $m)))))
                         (ref.cast $bignum (local.get $n))))))
             `((ref.test $bignum (local.get $m))
               ,(arith-cond
                 '((call $fixnum? (local.get $n))
                   (call $bignum-rem*
                         (ref.cast $bignum (local.get $m))
                         (ref.cast $bignum
                                   (call $i32->bignum
                                         (call $fixnum->i32
                                               (ref.cast i31
                                                         (local.get $n)))))))
                 '((ref.test $bignum (local.get $n))
                   (call $bignum-rem*
                         (ref.cast $bignum (local.get $m))
                         (ref.cast $bignum (local.get $n))))))))

     ;; floor of $M/$N, with $M, $N in Z and $N > 0 and both integers
     ;; normalized: (m - m mod n)/n, where m mod n = (% (+ (% m n) n) n)
     (func $fracfloor (param $m (ref eq)) (param $n (ref eq)) (result (ref eq))
           (call $div
                 (call $sub
                       (local.get $m)
                       (call $remz
                             (call $add
                                   (call $remz
                                         (local.get $m)
                                         (local.get $n))
                                   (local.get $n))
                             (local.get $n)))
                 (local.get $n)))

     (func $floor (param $x (ref eq)) (result (ref eq))
           ,(arith-cond
             '((call $fixnum? (local.get $x))
               (local.get $x))
             '((ref.test $bignum (local.get $x))
               (local.get $x))
             '((ref.test $flonum (local.get $x))
               (struct.new $flonum
                           (i32.const 0)
                           (f64.floor (call $flonum->f64 (ref.cast $flonum (local.get $x))))))
             '((ref.test $fraction (local.get $x))
               (call $fracfloor
                     (struct.get $fraction $num
                                 (ref.cast $fraction (local.get $x)))
                     (struct.get $fraction $denom
                                 (ref.cast $fraction (local.get $x)))))))

     (func $ceiling (param $x (ref eq)) (result (ref eq))
           ,(arith-cond
             '((call $fixnum? (local.get $x))
               (local.get $x))
             '((ref.test $bignum (local.get $x))
               (local.get $x))
             '((ref.test $flonum (local.get $x))
               (struct.new $flonum
                           (i32.const 0)
                           (f64.ceil (call $flonum->f64 (ref.cast $flonum (local.get $x))))))
             '((ref.test $fraction (local.get $x))
               (call $add
                 (call $floor (local.get $x))
                 (call $i32->fixnum (i32.const 1))))))

     (func $sqrt (param $x (ref eq)) (result (ref $flonum))
           ,(call-fmath '$fsqrt '(local.get $x)))

     (func $sin (param $x (ref eq)) (result (ref eq))
           ,(call-fmath '$fsin '(local.get $x)))
     (func $cos (param $x (ref eq)) (result (ref eq))
           ,(call-fmath '$fcos '(local.get $x)))
     (func $tan (param $x (ref eq)) (result (ref eq))
           ,(call-fmath '$ftan '(local.get $x)))
     (func $asin (param $x (ref eq)) (result (ref eq))
           ,(call-fmath '$fasin '(local.get $x)))
     (func $acos (param $x (ref eq)) (result (ref eq))
           ,(call-fmath '$facos '(local.get $x)))
     (func $atan (param $x (ref eq)) (result (ref eq))
           ,(call-fmath '$fatan '(local.get $x)))
     (func $atan2 (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
           ,(call-fmath '$fatan2 '(local.get $x) '(local.get $y)))
     (func $log (param $x (ref eq)) (result (ref eq))
           ,(call-fmath '$flog '(local.get $x)))
     (func $exp (param $x (ref eq)) (result (ref eq))
           ,(call-fmath '$fexp '(local.get $x)))

     (func $u64->bignum (param $i64 i64) (result (ref eq))
           (struct.new $bignum
                       (i32.const 0)
                       (call $bignum-from-u64 (local.get $i64))))
     (func $s64->bignum (param $i64 i64) (result (ref eq))
           (struct.new $bignum
                       (i32.const 0)
                       (call $bignum-from-i64 (local.get $i64))))
     (func $bignum->u64 (param $x (ref $bignum)) (result i64)
           (local $n (ref extern))
           (local.set $n (struct.get $bignum $val (local.get $x)))
           (if i64
               (call $bignum-is-u64 (local.get $n))
               (then (call $bignum-get-i64 (local.get $n)))
               (else
                (call $die (string.const "$bignum->u64 out of range")
                      (local.get $x))
                (unreachable))))
     (func $bignum->s64 (param $x (ref $bignum)) (result i64)
           (local $n (ref extern))
           (local.set $n (struct.get $bignum $val (local.get $x)))
           (if i64
               (call $bignum-is-i64 (local.get $n))
               (then (call $bignum-get-i64 (local.get $n)))
               (else
                (call $die (string.const "$bignum->s64 out of range")
                      (local.get $x))
                (unreachable))))

     (func $scm->s64 (param $a (ref eq)) (result i64)
           (if i64
               (call $fixnum? (local.get $a))
               (then
                (i64.extend_i32_s
                 (i32.shr_s (i31.get_s (ref.cast i31 (local.get $a)))
                            (i32.const 1))))
               (else
                (if i64
                    (ref.test $bignum (local.get $a))
                    (then
                     (return_call $bignum->s64
                                  (ref.cast $bignum (local.get $a))))
                    (else
                     (call $die (string.const "$scm->s64 bad arg")
                           (local.get $a))
                     (unreachable))))))
     (func $scm->u64 (param $a (ref eq)) (result i64)
           (local $i i32)
           (if i64
               (ref.test i31 (local.get $a))
               (then
                (local.set $i (i31.get_s (ref.cast i31 (local.get $a))))
                (if i64
                    (i32.and (local.get $i) (i32.const ,(logior 1 (ash -1 31))))
                    (then
                     (call $die
                           (string.const "$scm->u64 bad arg")
                           (local.get $a))
                     (unreachable))
                    (else
                     (i64.extend_i32_u
                      (i32.shr_u (local.get $i) (i32.const 1))))))
               (else
                (if i64
                    (ref.test $bignum (local.get $a))
                    (then
                     (return_call $bignum->u64
                                  (ref.cast $bignum (local.get $a))))
                    (else
                     (call $die
                           (string.const "$scm->u64 bad arg")
                           (local.get $a))
                     (unreachable))))))
     (func $scm->u64/truncate (param $a (ref eq)) (result i64)
           ,(arith-cond 'i64
             '((call $fixnum? (local.get $a))
               (i64.extend_i32_u
                (call $fixnum->i32 (ref.cast i31 (local.get $a)))))
             '((ref.test $bignum (local.get $a))
               (call $bignum-get-u64
                     (struct.get $bignum $val (ref.cast $bignum (local.get $a)))))
             '((i32.const 0)
               (call $die
                     (string.const "$scm->u64 bad arg")
                     (local.get $a))
               (unreachable))))
     (func $s64->scm (param $a i64) (result (ref eq))
           (if (result (ref eq))
               (i32.and (i64.ge_s (local.get $a) (i64.const ,(ash -1 29)))
                        (i64.lt_s (local.get $a) (i64.const ,(ash 1 29))))
               (then (ref.i31
                      (i32.shl (i32.wrap_i64 (local.get $a))
                               (i32.const 1))))
               (else (return_call $s64->bignum (local.get $a)))))
     (func $s32->scm (param $a i32) (result (ref eq))
           (if (ref eq)
               (i32.and (i32.ge_s (local.get $a) (i32.const ,(ash -1 29)))
                        (i32.lt_s (local.get $a) (i32.const ,(ash 1 29))))
               (then (call $i32->fixnum (local.get $a)))
               (else (return_call $s64->bignum (i64.extend_i32_s (local.get $a))))))

     (func $set-fluid-and-return-prev (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq))
           (param $arg2 (ref eq))
           (local $fluid (ref $fluid))
           (local $prev (ref eq))
           (if (i32.eqz (local.get $nargs))
               (then
                (return_call $wrong-num-args
                             (local.get $nargs)
                             (local.get $arg0)
                             (local.get $arg1)
                             (local.get $arg2))))
           (global.set $scm-sp (i32.sub (global.get $scm-sp) (i32.const 1)))
           (local.set $fluid
                      (ref.cast $fluid
                                (table.get $scm-stack (global.get $scm-sp))))
           (local.set $prev (call $fluid-ref (local.get $fluid)))
           (call $fluid-set! (local.get $fluid) (local.get $arg0))
           (global.set $ret-sp (i32.sub (global.get $ret-sp) (i32.const 1)))
           (return_call_ref $kvarargs
                            (i32.const 1)
                            (local.get $prev)
                            (ref.i31 (i32.const 1))
                            (ref.i31 (i32.const 1))
                            (table.get $ret-stack (global.get $ret-sp))))
     (func $parameter (param $nargs i32) (param $arg0 (ref eq))
           (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (local $parameter (ref $parameter))
           (local.set $parameter (ref.cast $parameter (local.get $arg0)))
           (if (i32.eq (local.get $nargs) (i32.const 1))
               (then
                (global.set $ret-sp
                            (i32.sub (global.get $ret-sp) (i32.const 1)))
                (return_call_ref $kvarargs
                                 (i32.const 1)
                                 (call $fluid-ref
                                       (struct.get $parameter $fluid
                                                   (local.get $parameter)))
                                 (ref.i31 (i32.const 1))
                                 (ref.i31 (i32.const 1))
                                 (table.get $ret-stack (global.get $ret-sp)))))
           (if (i32.ne (local.get $nargs) (i32.const 2))
               (then
                (return_call $wrong-num-args
                             (local.get $nargs)
                             (local.get $arg0)
                             (local.get $arg1)
                             (local.get $arg2))))
           (global.set $scm-sp (i32.add (global.get $scm-sp) (i32.const 1)))
           (call $maybe-grow-scm-stack)
           (global.set $ret-sp (i32.add (global.get $ret-sp) (i32.const 1)))
           (call $maybe-grow-ret-stack)
           (table.set $scm-stack (i32.sub (global.get $scm-sp) (i32.const 1))
                      (struct.get $parameter $fluid (local.get $parameter)))
           (table.set $ret-stack (i32.sub (global.get $ret-sp) (i32.const 1))
                      (ref.func $set-fluid-and-return-prev))
           (return_call_ref $kvarargs
                            (i32.const 2)
                            (struct.get $parameter $convert
                                        (local.get $parameter))
                            (local.get $arg1)
                            (ref.i31 (i32.const 1))
                            (struct.get $proc $func
                                        (struct.get $parameter $convert
                                                    (local.get $parameter)))))

     (table ,@(maybe-import '$argv) 0 (ref null eq))
     (table ,@(maybe-import '$scm-stack) 0 (ref null eq))
     (table ,@(maybe-import '$ret-stack) 0 (ref null $kvarargs))
     (table ,@(maybe-import '$dyn-stack) 0 (ref null $dyn))

     (memory ,@(maybe-import '$raw-stack) 0)

     (global ,@(maybe-import '$arg3) (mut (ref eq)) ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$arg4) (mut (ref eq)) ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$arg5) (mut (ref eq)) ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$arg6) (mut (ref eq)) ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$arg7) (mut (ref eq)) ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$ret-sp) (mut i32) ,@maybe-init-i32-zero)
     (global ,@(maybe-import '$scm-sp) (mut i32) ,@maybe-init-i32-zero)
     (global ,@(maybe-import '$raw-sp) (mut i32) ,@maybe-init-i32-zero)
     (global ,@(maybe-import '$dyn-sp) (mut i32) ,@maybe-init-i32-zero)
     (global ,@(maybe-import '$current-fluids) (mut (ref $hash-table))
             ,@maybe-init-hash-table)
     (global ,@(maybe-import '$raise-exception) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$with-exception-handler) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$current-input-port) (mut (ref eq))
             ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$current-output-port) (mut (ref eq))
             ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$current-error-port) (mut (ref eq))
             ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$default-prompt-tag) (mut (ref eq))
             ,@maybe-init-i31-zero))))
