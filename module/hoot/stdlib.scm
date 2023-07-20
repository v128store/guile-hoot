;;; Standard library for Hoot runtime
;;; Copyright (C) 2023 Igalia, S.L.
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
  #:export (compute-stdlib))

(define (compute-stdlib import-abi?)
  (define (maybe-import id)
    (if import-abi?
        `(,id (import "abi" ,(symbol->string id)))
        `(,id)))

  (define maybe-init-i31-zero
    (if import-abi?
        '()
        '((i31.new (i32.const 0)))))
  (define maybe-init-i32-zero
    (if import-abi?
        '()
        '((i32.const 0))))

  (parse-wat
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
            (struct
             (field $hash (mut i32))))

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
               (field $size (mut (ref i31)))
               (field $buckets (ref $vector)))))
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
               (field $val (ref extern)))))
      (type $syntax
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $expr (ref eq))
               (field $wrap (ref eq))
               (field $module (ref eq))
               (field $source (ref eq)))))
      (type $port-type
            (struct
             (field $name (ref string))
             ;; in guile these are (port, bv, start, count) -> size_t
             (field $read (ref null $proc)) ;; could have a more refined type
             (field $write (ref null $proc))
             (field $seek (ref null $proc)) ;; (port, offset, whence) -> offset
             (field $close (ref null $proc)) ;; (port) -> ()
             (field $get-natural-buffer-sizes (ref null $proc)) ;; port -> (rdsz, wrsz)
             (field $random-access? (ref null $proc)) ;; port -> bool
             (field $input-waiting (ref null $proc))  ;; port -> bool
             (field $truncate (ref null $proc)) ;; (port, length) -> ()
             ;; Guile also has GOOPS classes here.
             ))
      (type $port
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $pt (ref $port-type))
               (field $stream (mut (ref eq)))
               (field $file_name (mut (ref eq)))
               (field $position (ref $mutable-pair))
               (field $read_buf (mut (ref eq)))      ;; A 5-vector
               (field $write_buf (mut (ref eq)))     ;; A 5-vector
               (field $write_buf_aux (mut (ref eq))) ;; A 5-vector
               (field $read_buffering (mut i32))
               (field $refcount (mut i32))
               (field $rw_random (mut i8))
               (field $properties (mut (ref eq))))))
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

      (type $dyn (struct))
      (type $dynwind
            (sub $dyn
              (struct
               (field $raw-sp i32)
               (field $scm-sp i32)
               (field $ret-sp i32)
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
               (field $state (mut (ref eq)))))))

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

     (func $fsin (import "rt" "fsin") (param f64) (result f64))
     (func $fcos (import "rt" "fcos") (param f64) (result f64))
     (func $ftan (import "rt" "ftan") (param f64) (result f64))
     (func $fasin (import "rt" "fasin") (param f64) (result f64))
     (func $facos (import "rt" "facos") (param f64) (result f64))
     (func $fatan (import "rt" "fatan") (param f64) (result f64))
     (func $fatan2 (import "rt" "fatan2") (param f64 f64) (result f64))

     (func $die (import "rt" "die")
           (param (ref string) (ref eq)))

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

     ;; Mix hash bits.  Result must be nonzero.
     (func $finish-heap-object-hash (param $hash i32) (result i32)
           (local.set $hash (call $integer-hash (local.get $hash)))
           (if i32 (local.get $hash)
               (then (local.get $hash))
               (else (call $integer-hash (i32.const 42)))))

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

     ;; FIXME: Replace with version from basic-types.wat.
     (func $string->symbol (param $str (ref string)) (result (ref $symbol))
           (local.get 0) (call $string-hash) (call $finish-heap-object-hash)
           (i32.const 0) (local.get 0) (struct.new $string)
           (struct.new $symbol)
           (call $intern-symbol!))

     (func $intern-symbol! (param $sym (ref $symbol)) (result (ref $symbol))
           ;; FIXME: Actually interning into symtab is unimplemented!
           (local.get 0))

     (func $symbol->keyword (param $sym (ref $symbol)) (result (ref $keyword))
           ;; FIXME: intern into kwtab.
           (local.get 0) (struct.get $symbol 0) (call $finish-heap-object-hash)
           (local.get 0)
           (struct.new $keyword))

     (func $grow-raw-stack (param $sp i32)
           ;; Grow the stack by at least 50% and at least the needed
           ;; space.  Trap if we fail to grow.
           ;; additional_size = (current_size >> 1) | needed_size
           (memory.size $raw-stack)
           (i32.const 1)
           (i32.shr_u)
           (local.get $sp)
           (i32.const 16) ;; Wasm pages are 64 kB.
           (i32.shr_u)
           (i32.or)
           (memory.grow $raw-stack)
           (i32.const -1)
           (i32.eq)
           (if (i32.eq) (then (unreachable))))

     (func $grow-scm-stack (param $sp i32)
           ;; Grow as in $grow-raw-stack.
           (i32.const 0)
           (i31.new)
           (table.size $scm-stack)
           (i32.const 1)
           (i32.shr_u)
           (local.get $sp)
           (i32.or)
           (table.grow $scm-stack)
           (i32.const -1)
           (if (i32.eq) (then (unreachable))))

     (func $invalid-continuation (type $kvarargs) (unreachable))
     (func $grow-ret-stack (param $sp i32)
           ;; Grow as in $grow-raw-stack.
           (ref.func $invalid-continuation)
           (table.size $ret-stack)
           (i32.const 1)
           (i32.shr_u)
           (local.get $sp)
           (i32.or)
           (table.grow $ret-stack)
           (i32.const -1)
           (if (i32.eq) (then (unreachable))))

     (func $grow-dyn-stack
           ;; Grow as in $grow-raw-stack.
           (ref.null $dyn)
           (i32.add (i32.shr_u (table.size $dyn-stack) (i32.const 1))
                    (i32.const 1))
           (table.grow $dyn-stack)
           (i32.const -1)
           (if (i32.eq) (then (unreachable))))

     (func $wrong-num-args (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (call $die (string.const "wrong-number-of-args") (local.get $arg0))
           (unreachable))

     (func $collect-rest-args (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (param $npositional i32)
           (result (ref eq))
           (local $ret (ref eq))
           (local.set $ret (i31.new (i32.const 13))) ;; null
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

     (func $arg-ref (param $n i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (result (ref eq))
           (block
            $n0
            (block
             $n1
             (block
              $n2
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
                    (br_table $n0
                              $n1
                              $n2
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
             (return (local.get $arg2)))
            (return (local.get $arg1)))
           (return (local.get $arg0)))

     (func $collect-apply-args
           (param $nargs i32) (param $arg0 (ref eq))
           (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (result (ref eq))
           (local $ret (ref eq))
           (local.set $ret
                      (call $arg-ref
                            (local.tee $nargs
                                       (i32.sub (local.get $nargs)
                                                (i32.const 1)))
                            (local.get $arg0)
                            (local.get $arg1)
                            (local.get $arg2)))
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
                                                        (i32.const 1)))
                                    (local.get $arg0)
                                    (local.get $arg1)
                                    (local.get $arg2))
                              (local.get $ret)))
                  (br $lp))))
           (local.get $ret))

     (func $apply-to-non-list (param $tail (ref eq))
           (unreachable))
     (func $get-callee-code (param $callee (ref eq)) (result (ref $kvarargs))
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
                                      (local.get $arg0)
                                      (local.get $arg1)
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
           (if (i32.eqz (ref.eq (local.get $args) (i31.new (i32.const 13))))
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

     (func $slow-< (param $a (ref eq)) (param $b (ref eq)) (result i32)
           (unreachable))
     (func $slow-<= (param $a (ref eq)) (param $b (ref eq)) (result i32)
           (unreachable))
     (func $slow-= (param $a (ref eq)) (param $b (ref eq)) (result i32)
           (unreachable))

     (func $string-set! (param $str (ref $string)) (param $idx i32)
           (param $ch i32)
           (unreachable))

     (func $add (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           (unreachable))
     (func $sub (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           (unreachable))
     (func $add/immediate (param $a (ref eq)) (param $b i32) (result (ref eq))
           (unreachable))
     (func $sub/immediate (param $a (ref eq)) (param $b i32) (result (ref eq))
           (unreachable))
     (func $mul (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           (unreachable))
     (func $div (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           (unreachable))
     (func $quo (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           (unreachable))
     (func $rem (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           (unreachable))
     (func $mod (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           (unreachable))

     (func $logand (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           (unreachable))
     (func $logior (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           (unreachable))
     (func $logxor (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           (unreachable))
     (func $logsub (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
           (unreachable))

     (func $rsh (param $a (ref eq)) (param $b i64) (result (ref eq))
           (unreachable))
     (func $lsh (param $a (ref eq)) (param $b i64) (result (ref eq))
           (unreachable))

     (func $inexact (param $x (ref eq)) (result (ref eq))
           (unreachable))
     (func $abs (param $x (ref eq)) (result (ref eq))
           (unreachable))
     (func $sqrt (param $x (ref eq)) (result (ref eq))
           (unreachable))
     (func $floor (param $x (ref eq)) (result (ref eq))
           (unreachable))
     (func $ceiling (param $x (ref eq)) (result (ref eq))
           (unreachable))

     (func $sin (param $x (ref eq)) (result (ref eq))
           (unreachable))
     (func $cos (param $x (ref eq)) (result (ref eq))
           (unreachable))
     (func $tan (param $x (ref eq)) (result (ref eq))
           (unreachable))
     (func $asin (param $x (ref eq)) (result (ref eq))
           (unreachable))
     (func $acos (param $x (ref eq)) (result (ref eq))
           (unreachable))
     (func $atan (param $x (ref eq)) (result (ref eq))
           (unreachable))
     (func $atan2 (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
           (unreachable))

     (func $u64->bignum (param $i64 i64) (result (ref eq))
           (unreachable))
     (func $s64->bignum (param $i64 i64) (result (ref eq))
           (unreachable))

     (func $scm->u64 (param $a (ref eq)) (result i64)
           (unreachable))
     (func $scm->u64/truncate (param $a (ref eq)) (result i64)
           (unreachable))
     (func $s64->scm (param $a i64) (result (ref eq))
           (if (result (ref eq))
               (i32.and (i64.ge_s (local.get $a) (i64.const ,(ash -1 29)))
                        (i64.lt_s (local.get $a) (i64.const ,(ash 1 29))))
               (then (i31.new
                      (i32.shl (i32.wrap_i64 (local.get $a))
                               (i32.const 1))))
               (else (return_call $s64->bignum (local.get $a)))))

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
     (global ,@(maybe-import '$dyn-sp) (mut i32) ,@maybe-init-i32-zero))))
