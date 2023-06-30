;; The reflect module has four purposes:
;;
;;  1. Let embedders (e.g. JS) to know what the type of an object is
;;
;;  2. Let embedders access an object's value (e.g. a flonum's value)
;;
;;  3. Let embedders construct "value-like" objects: fixnums, bignums,
;;     flonums, compnums, fractions, immutable strings, symbols,
;;     immutable pairs, keywords.  Possibly immutable bytevectors and
;;     bitvectors also.
;;
;;  4. Let embedders call exported functions and receive their return
;;     values.
;;
;; To do this, this module needs some shared state from the Scheme
;; system:  storage locations used for passing parameters and return
;; values, the continuation stack (to allow for calling), and
;; string->symbol and string->keyword procedures needed to construct
;; symbols and keywords.

(module
  (type $kvarargs
    (func (param $nargs i32)
          (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))))

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
        (field $input-waiting (ref null $proc)) ;; port -> bool
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
          (field $read_buf (mut (ref eq)))  ;; A 5-vector
          (field $write_buf (mut (ref eq))) ;; A 5-vector
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
          (field $field3 (mut (ref eq)))))))

  ;; Storage locations for parameters and stack-allocated continuations.
  ;; Note that $arg0, $arg1, and $arg2 are function parameters.
  (global $arg3 (import "abi" "$arg3") (mut (ref eq)))
  (global $arg4 (import "abi" "$arg4") (mut (ref eq)))
  (global $arg5 (import "abi" "$arg5") (mut (ref eq)))
  (global $arg6 (import "abi" "$arg6") (mut (ref eq)))
  (global $arg7 (import "abi" "$arg7") (mut (ref eq)))
  (table $argv (import "abi" "$argv") 0 (ref null eq))

  (global $raw-sp (import "abi" "$raw-sp") (mut i32))
  (global $scm-sp (import "abi" "$scm-sp") (mut i32))
  (global $ret-sp (import "abi" "$ret-sp") (mut i32))

  (memory $raw-stack (import "abi" "$raw-stack") 0)
  (table $scm-stack (import "abi" "$scm-stack") 0 (ref null eq))
  (table $ret-stack (import "abi" "$ret-stack") 0 (ref null $kvarargs))

  (func $string->symbol (import "abi" "$string->symbol")
        (param $str (ref string)) (result (ref $symbol)))
  (func $symbol->keyword (import "abi" "$symbol->keyword")
        (param $str (ref $symbol)) (result (ref $keyword)))

  (func $abort (type $kvarargs) (unreachable))

  ;; These grow functions try to grow their corresponding tables,
  ;; filling in a sensible default value so as to not require the
  ;; tables to be nullable, and abort if that fails.
  (func $grow-argv (param $diff i32)
    (br_if 0 (i32.le_s (i32.const 0)
                       (table.grow $argv (i31.new (i32.const 0))
                                   (local.get $diff))))
    (unreachable))
  (func $grow-ret-stack (param $diff i32)
    (br_if 0 (i32.le_s (i32.const 0)
                       (table.grow $ret-stack (ref.func $abort)
                                   (local.get $diff))))
    (unreachable))

  (func $describe (export "describe") (param $scm (ref eq)) (result (ref string))
    (local $tmp i32)
    (block $Ldone (result (ref string))
      ;; Verrrrry annoyingly, binaryen doesn't support all of WAT and
      ;; specifically doesn't do implicit stack arguments, so we can't
      ;; use br_on_cast as we would like.
      (block $Limmediate
        (block $Lflonum
          (block $Lbignum
            (block $Lcomplex
              (block $Lfraction
                (block $Lpair
                  (block $Lmutable-pair
                    (block $Lvector
                      (block $Lmutable-vector
                        (block $Lbytevector
                          (block $Lmutable-bytevector
                            (block $Lbitvector
                              (block $Lmutable-bitvector
                                (block $Lstring
                                  (block $Lmutable-string
                                    (block $Lproc
                                      (block $Lsymbol
                                        (block $Lkeyword
                                          (block $Lvariable
                                            (block $Latomic-box
                                              (block $Lhash-table
                                                (block $Lweak-table
                                                  (block $Lfluid
                                                    (block $Ldynamic-state
                                                      (block $Lsyntax
                                                        (block $Lport
                                                          (block $Lstruct
                                                            (br_if $Limmediate (ref.test i31 (local.get $scm)))
                                                            (br_if $Lflonum (ref.test $flonum (local.get $scm)))
                                                            (br_if $Lbignum (ref.test $bignum (local.get $scm)))
                                                            (br_if $Lcomplex (ref.test $complex (local.get $scm)))
                                                            (br_if $Lfraction (ref.test $fraction (local.get $scm)))
                                                            (br_if $Lmutable-pair (ref.test $mutable-pair (local.get $scm)))
                                                            (br_if $Lpair (ref.test $pair (local.get $scm)))
                                                            (br_if $Lmutable-vector (ref.test $mutable-vector (local.get $scm)))
                                                            (br_if $Lvector (ref.test $vector (local.get $scm)))
                                                            (br_if $Lmutable-bytevector (ref.test $mutable-bytevector (local.get $scm)))
                                                            (br_if $Lbytevector (ref.test $bytevector (local.get $scm)))
                                                            (br_if $Lmutable-bitvector (ref.test $mutable-bitvector (local.get $scm)))
                                                            (br_if $Lbitvector (ref.test $bitvector (local.get $scm)))
                                                            (br_if $Lmutable-string (ref.test $mutable-string (local.get $scm)))
                                                            (br_if $Lstring (ref.test $string (local.get $scm)))
                                                            (br_if $Lproc (ref.test $proc (local.get $scm)))
                                                            (br_if $Lsymbol (ref.test $symbol (local.get $scm)))
                                                            (br_if $Lkeyword (ref.test $keyword (local.get $scm)))
                                                            (br_if $Lvariable (ref.test $variable (local.get $scm)))
                                                            (br_if $Latomic-box (ref.test $atomic-box (local.get $scm)))
                                                            (br_if $Lhash-table (ref.test $hash-table (local.get $scm)))
                                                            (br_if $Lweak-table (ref.test $weak-table (local.get $scm)))
                                                            (br_if $Lfluid (ref.test $fluid (local.get $scm)))
                                                            (br_if $Ldynamic-state (ref.test $dynamic-state (local.get $scm)))
                                                            (br_if $Lsyntax (ref.test $syntax (local.get $scm)))
                                                            (br_if $Lport (ref.test $port (local.get $scm)))
                                                            (br_if $Lstruct (ref.test $struct (local.get $scm)))
                                                            (unreachable))
                                                          (br $Ldone (string.const "struct")))
                                                        (br $Ldone (string.const "port")))
                                                      (br $Ldone (string.const "syntax")))
                                                    (br $Ldone (string.const "dynamic-state")))
                                                  (br $Ldone (string.const "fluid")))
                                                (br $Ldone (string.const "weak-table")))
                                              (br $Ldone (string.const "hash-table")))
                                            (br $Ldone (string.const "atomic-box")))
                                          (br $Ldone (string.const "variable")))
                                        (br $Ldone (string.const "keyword")))
                                      (br $Ldone (string.const "symbol")))
                                    (br $Ldone (string.const "procedure")))
                                  (br $Ldone (string.const "mutable-string")))
                                (br $Ldone (string.const "string")))
                              (br $Ldone (string.const "mutable-bitvector")))
                            (br $Ldone (string.const "bitvector")))
                          (br $Ldone (string.const "mutable-bytevector")))
                        (br $Ldone (string.const "bytevector")))
                      (br $Ldone (string.const "mutable-vector")))
                    (br $Ldone (string.const "vector")))
                  (br $Ldone (string.const "mutable-pair")))
                (br $Ldone (string.const "pair")))
              (br $Ldone (string.const "fraction")))
            (br $Ldone (string.const "complex")))
          (br $Ldone (string.const "bignum")))
        (br $Ldone (string.const "flonum")))
      ;; Immediate.
      (block $Lfalse
        (block $Lnil
          (block $Lnull
            (block $Ltrue
              (block $Lunspecified
                (block $Leof
                  (block $Lchar
                    (block $Lfixnum
                      (local.set $tmp (i31.get_s (ref.cast i31 (local.get $scm))))
                      (br_if $Lfixnum (i32.eqz (i32.and (local.get $tmp) (i32.const 1))))
                      (br_if $Lchar (i32.eq (i32.and (local.get $tmp) (i32.const 3))
                                            (i32.const 3)))
                      (br_if $Lfalse (i32.eq (local.get $tmp) (i32.const 1)))
                      (br_if $Lnil (i32.eq (local.get $tmp) (i32.const 5)))
                      (br_if $Lnull (i32.eq (local.get $tmp) (i32.const 13)))
                      (br_if $Ltrue (i32.eq (local.get $tmp) (i32.const 17)))
                      (br_if $Lunspecified (i32.eq (local.get $tmp) (i32.const 33)))
                      (br_if $Leof (i32.eq (local.get $tmp) (i32.const 41)))
                      (unreachable))
                    (br $Ldone (string.const "fixnum")))
                  (br $Ldone (string.const "char")))
                (br $Ldone (string.const "eof")))
              (br $Ldone (string.const "unspecified")))
            (br $Ldone (string.const "true")))
          (br $Ldone (string.const "null")))
        (br $Ldone (string.const "nil")))
      (br $Ldone (string.const "false"))))

  (func $scm-most-negative-fixnum (export "scm_most_negative_fixnum") (result i32)
    (i32.const -536870912)) ;; = -0x20000000
  (func $scm-most-positive-fixnum (export "scm_most_positive_fixnum") (result i32)
    (i32.const 536870911)) ;; = 0x1fffffff
  (func $scm-from-fixnum (export "scm_from_fixnum") (param $v i32) (result (ref eq))
    (i31.new (i32.shl (local.get $v) (i32.const 1))))
  (func $scm-from-bignum (export "scm_from_bignum") (param $v (ref extern)) (result (ref eq))
    (struct.new $bignum (i32.const 0) (local.get $v)))
  (func $scm-from-f64 (export "scm_from_f64") (param $v f64) (result (ref $flonum))
    (struct.new $flonum (i32.const 0) (local.get $v)))
  (func $scm-false (export "scm_false") (result (ref i31))
    (i31.new (i32.const 1)))
  (func $scm-nil (export "scm_nil") (result (ref i31))
    (i31.new (i32.const 5)))
  (func $scm-null (export "scm_null") (result (ref i31))
    (i31.new (i32.const 13)))
  (func $scm-true (export "scm_true") (result (ref i31))
    (i31.new (i32.const 17)))
  (func $scm-unspecified (export "scm_unspecified") (result (ref i31))
    (i31.new (i32.const 33)))
  (func $scm-eof (export "scm_eof") (result (ref i31))
    (i31.new (i32.const 41)))
  (func $scm-from-char (export "scm_from_char") (param $ch i32) (result (ref i31))
    (i31.new (i32.and (i32.const 3)
                      (i32.shl (local.get $ch) (i32.const 2)))))
  (func $scm-from-fraction (export "scm_from_fraction") (param (ref eq) (ref eq)) (result (ref $fraction))
    ;; FIXME: check types.
    (struct.new $fraction (i32.const 0) (local.get 0) (local.get 1)))
  (func $scm-from-complex (export "scm_from_complex") (param f64 f64) (result (ref $complex))
    ;; FIXME: check types?
    (struct.new $complex (i32.const 0) (local.get 0) (local.get 1)))
  (func $scm-from-string (export "scm_from_string") (param $str (ref string)) (result (ref $string))
    (struct.new $string (i32.const 0) (local.get $str)))

  (func $fixnum_value (export "fixnum_value") (param $v (ref i31)) (result i32)
    (i32.shr_s (i31.get_s (local.get $v)) (i32.const 1)))
  (func $char_value (export "char_value") (param $v (ref i31)) (result i32)
    (i32.shr_u (i31.get_s (local.get $v)) (i32.const 2)))
  (func $bignum_value (export "bignum_value") (param $v (ref $bignum)) (result (ref extern))
    (struct.get $bignum 1 (local.get $v)))
  (func $flonum_value (export "flonum_value") (param $v (ref $flonum)) (result f64)
    (struct.get $flonum 1 (local.get $v)))
  ;; FIXME: Should return 2 values but binaryen doesn't support that :-(
  (func $fraction-num (export "fraction_num") (param $v (ref $fraction)) (result (ref eq))
    (struct.get $fraction 1 (local.get $v)))
  (func $fraction-denom (export "fraction_denom") (param $v (ref $fraction)) (result (ref eq))
    (struct.get $fraction 2 (local.get $v)))
  ;; FIXME: Should return 2 values but binaryen doesn't support that :-(
  (func $complex-real (export "complex_real") (param $v (ref $complex)) (result f64)
    (struct.get $complex 1 (local.get $v)))
  (func $complex-imag (export "complex_imag") (param $v (ref $complex)) (result f64)
    (struct.get $complex 2 (local.get $v)))
  (func $string_value (export "string_value") (param $v (ref $string)) (result (ref string))
    (struct.get $string 1 (local.get $v)))
  (func $symbol_name (export "symbol_name") (param $v (ref $symbol)) (result (ref string))
    (call $string_value (struct.get $symbol $name (local.get $v))))
  (func $keyword_name (export "keyword_name") (param $v (ref $keyword)) (result (ref string))
    (call $symbol_name (struct.get $keyword $name (local.get $v))))
  (func $pair_car (export "car") (param $v (ref $pair)) (result (ref eq))
    (struct.get $pair 1 (local.get $v)))
  (func $pair_cdr (export "cdr") (param $v (ref $pair)) (result (ref eq))
    (struct.get $pair 2 (local.get $v)))

  (func $push-return (param $k (ref $kvarargs))
    (local $sp i32)
    (local.set $sp (global.get $ret-sp))
    (if (i32.eq (table.size $ret-stack) (local.get $sp))
        (then (call $grow-ret-stack
                    (i32.add (i32.const 16) (local.get $sp)))))
    (table.set $ret-stack (local.get $sp) (local.get $k))
    (global.set $ret-sp (i32.add (local.get $sp) (i32.const 1))))

  (func $make-vector (export "make_vector")
        (param $size i32) (param $init (ref eq)) (result (ref $mutable-vector))
    (struct.new $mutable-vector
                (i32.const 0)
                (array.new $raw-scmvector (local.get $init) (local.get $size))))
  (func $vector-set (export "vector_set")
        (param $vec (ref $mutable-vector)) (param $i i32) (param $val (ref eq))
    (array.set $raw-scmvector (struct.get $mutable-vector $vals (local.get $vec))
               (local.get $i) (local.get $val)))
  (func $vector-ref (export "vector_ref")
        (param $vec (ref $vector)) (param $i i32) (result (ref eq))
    (array.get $raw-scmvector (struct.get $vector $vals (local.get $vec))
               (local.get $i)))
  (func $vector-length (export "vector_length")
        (param $vec (ref $vector)) (result i32)
    (array.len (struct.get $vector $vals (local.get $vec))))

  (func $bytevector-set (export "bytevector_set")
        (param $vec (ref $mutable-bytevector)) (param $i i32) (param $val i32)
    (array.set $raw-bytevector (struct.get $mutable-bytevector $vals (local.get $vec))
               (local.get $i) (local.get $val)))
  (func $bytevector-ref (export "bytevector_ref")
        (param $vec (ref $bytevector)) (param $i i32) (result i32)
    (array.get_u $raw-bytevector (struct.get $bytevector $vals (local.get $vec))
                 (local.get $i)))
  (func $bytevector-length (export "bytevector_length")
        (param $vec (ref $bytevector)) (result i32)
    (array.len (struct.get $bytevector $vals (local.get $vec))))

  (func $bitvector-ref (export "bitvector_ref")
        (param $vec (ref $bitvector)) (param $i i32) (result i32)
    (i32.and
     (i32.shr_u
      (array.get $raw-bitvector (struct.get $bitvector $vals (local.get $vec))
                 (i32.shr_u (local.get $i) (i32.const 5)))
      (local.get $i))
     (i32.const 1)))
  (func $bitvector-length (export "bitvector_length")
        (param $vec (ref $bitvector)) (result i32)
    (struct.get $bitvector $len (local.get $vec)))

  (global $return-values (mut (ref null $raw-scmvector)) (ref.null $raw-scmvector))

  (func $save-values-and-return-to-host (param $nargs i32)
        (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
    (local $i i32)
    (local $ret (ref $raw-scmvector))
    (local.set $ret (array.new $raw-scmvector (i31.new (i32.const 1))
                               (local.get $nargs)))
    (block $nargs0
      (block $nargs1
        (block $nargs2
          (block $nargs3
            (block $nargs4
              (block $nargs5
                (block $nargs6
                  (block $nargs7
                    (block $nargs8
                      (block $nargsN
                        (br_table $nargs0
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
                      (local.set $i (i32.const 8))
                      (loop
                       (array.set $raw-scmvector (local.get $ret) (local.get $i)
                                  (ref.as_non_null
                                   (table.get $argv
                                              (i32.sub (local.get $i) (i32.const 8)))))
                       (local.set $i (i32.add (local.get $i) (i32.const 1)))
                       (br_if 0 (i32.lt_s (local.get $i) (local.get $nargs)))))
                    (array.set $raw-scmvector (local.get $ret) (i32.const 7)
                               (global.get $arg7)))
                  (array.set $raw-scmvector (local.get $ret) (i32.const 6)
                             (global.get $arg6)))
                (array.set $raw-scmvector (local.get $ret) (i32.const 5)
                           (global.get $arg5)))
              (array.set $raw-scmvector (local.get $ret) (i32.const 4)
                         (global.get $arg4)))
            (array.set $raw-scmvector (local.get $ret) (i32.const 3)
                       (global.get $arg3)))
          (array.set $raw-scmvector (local.get $ret) (i32.const 2)
                     (local.get $arg2)))
        (array.set $raw-scmvector (local.get $ret) (i32.const 1)
                   (local.get $arg1)))
      (array.set $raw-scmvector (local.get $ret) (i32.const 0)
                 (local.get $arg0)))
    (global.set $return-values (local.get $ret)))

  (func $call (export "call")
        (param $proc (ref $proc)) (param $args (ref $vector))
        (result (ref $vector))
    (local $arg0 (ref eq))
    (local $arg1 (ref eq))
    (local $arg2 (ref eq))
    (local $vals (ref $raw-scmvector))
    (local $i i32)
    (local.set $vals (struct.get $vector $vals (local.get $args)))
    (local.set $arg0 (i31.new (i32.const 0)))
    (local.set $arg1 (i31.new (i32.const 0)))
    (local.set $arg2 (i31.new (i32.const 0)))
    (block $nargs0
      (block $nargs1
        (block $nargs2
          (block $nargs3
            (block $nargs4
              (block $nargs5
                (block $nargs6
                  (block $nargs7
                    (block $nargs8
                      (block $nargsN
                        (br_table $nargs0
                                  $nargs1
                                  $nargs2
                                  $nargs3
                                  $nargs4
                                  $nargs5
                                  $nargs6
                                  $nargs7
                                  $nargs8
                                  $nargsN
                                  (array.len (local.get $vals))))
                      (local.set $i (i32.const 8))
                      (loop
                       (table.set $argv
                                  (i32.sub (local.get $i) (i32.const 8))
                                  (array.get $raw-scmvector
                                             (local.get $vals) (local.get $i)))
                       (local.set $i (i32.add (local.get $i) (i32.const 1)))
                       (br_if 0 (i32.lt_s (local.get $i)
                                          (array.len (local.get $vals))))))
                    (global.set $arg7 (array.get $raw-scmvector
                                                 (local.get $vals) (i32.const 7))))
                  (global.set $arg6 (array.get $raw-scmvector
                                               (local.get $vals) (i32.const 6))))
                (global.set $arg5 (array.get $raw-scmvector
                                             (local.get $vals) (i32.const 5))))
              (global.set $arg4 (array.get $raw-scmvector
                                           (local.get $vals) (i32.const 4))))
            (global.set $arg3 (array.get $raw-scmvector
                                         (local.get $vals) (i32.const 3))))
          (local.set $arg2 (array.get $raw-scmvector
                                      (local.get $vals) (i32.const 2))))
        (local.set $arg1 (array.get $raw-scmvector
                                    (local.get $vals) (i32.const 1))))
      (local.set $arg0 (array.get $raw-scmvector
                                  (local.get $vals) (i32.const 0))))
    (call $push-return (ref.func $save-values-and-return-to-host))
    (call_ref $kvarargs (array.len (local.get $vals))
              (local.get $arg0) (local.get $arg1) (local.get $arg2)
              (struct.get $proc $func (local.get $proc)))
    (local.set $vals (ref.as_non_null (global.get $return-values)))
    (global.set $return-values (ref.null $raw-scmvector))
    (struct.new $vector (i32.const 0) (local.get $vals))))
