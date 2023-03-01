(module
  (type $kvarargs (func (param i32) (result i32)))

  (type $raw-immutable-bitvector (array i32))
  (type $raw-immutable-bytevector (array i8))

  (type $raw-bitvector (array (mut i32)))
  (type $raw-bytevector (array (mut i8)))
  (type $raw-scmvector (array (mut (ref eq))))

  (type $void-struct (struct))
  (type $heap-object
    (sub $void-struct
      (struct
        (field $tag-and-hash (mut i32)))))

  (type $extern-ref
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $val (ref extern)))))

  ;; Bignums are $extern-ref.
  (type $flonum
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $val f64))))
  (type $complex
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $real f64)
        (field $imag f64))))
  (type $fraction
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $num (ref eq))
        (field $denom (ref eq)))))
  (type $pair
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $car (mut (ref eq)))
        (field $cdr (mut (ref eq))))))
  (type $vector
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $vals (ref $raw-scmvector)))))
  (type $bytevector
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $vals (ref $raw-bytevector)))))
  (type $bitvector
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $len i32)
        (field $vals (ref $raw-bitvector)))))
  (type $string
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $str (ref string)))))
  (type $proc
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $func (ref $kvarargs)))))
  (type $symbol
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $name (ref $string)))))
  (type $keyword
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $name (ref $symbol)))))
  (type $box
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $val (mut (ref eq))))))
  (type $hash-table
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $size (mut (ref i31)))
        (field $buckets (ref $vector)))))
  (type $fluid
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
        (field $init (ref eq)))))
  ;; Dynamic states are $extern-ref to host weak map.
  (type $syntax
    (sub $heap-object
      (struct
        (field $tag-and-hash (mut i32))
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
        (field $tag-and-hash (mut i32))
        (field $pt (ref $port-type))
        (field $stream (mut (ref eq)))
        (field $file_name (mut (ref eq)))
        (field $position (ref $pair))
        (field $read_buf (mut (ref eq)))  ;; A 5-vector
        (field $write_buf (mut (ref eq))) ;; A 5-vector
        (field $write_buf_aux (mut (ref eq))) ;; A 5-vector
        (field $read_buffering (mut i32))
        (field $refcount (mut i32))
        (field $rw_random (mut i8))
        (field $properties (mut (ref eq))))))

  ;; Vtable link is mutable so that we can tie the knot for top types.
  (rec
    (type $struct
      (sub $heap-object
        (struct
          (field $tag-and-hash (mut i32))
          (field $vtable (mut (ref null $struct4))))))
    (type $struct1
      (sub $struct
        (struct
          (field $tag-and-hash (mut i32))
          (field $vtable (mut (ref null $struct4)))
          (field $field0 (mut (ref eq))))))
    (type $struct2
      (sub $struct1
        (struct
          (field $tag-and-hash (mut i32))
          (field $vtable (mut (ref null $struct4)))
          (field $field0 (mut (ref eq)))
          (field $field1 (mut (ref eq))))))
    (type $struct3
      (sub $struct2
        (struct
          (field $tag-and-hash (mut i32))
          (field $vtable (mut (ref null $struct4)))
          (field $field0 (mut (ref eq)))
          (field $field1 (mut (ref eq)))
          (field $field2 (mut (ref eq))))))
    (type $struct4
      (sub $struct3
        (struct
          (field $tag-and-hash (mut i32))
          (field $vtable (mut (ref null $struct4)))
          (field $field0 (mut (ref eq)))
          (field $field1 (mut (ref eq)))
          (field $field2 (mut (ref eq)))
          (field $field3 (mut (ref eq))))))
    (type $structN
      (sub $struct4
        (struct
          (field $tag-and-hash (mut i32))
          (field $vtable (mut (ref null $struct4)))
          (field $field0 (mut (ref eq)))
          (field $field1 (mut (ref eq)))
          (field $field2 (mut (ref eq)))
          (field $field3 (mut (ref eq)))
          (field $tail (ref $raw-scmvector))))))

  ;; FIXME: Probably we should have non-nullable types here but binaryen
  ;; doesn't support it.
  (table $argv 0 (ref null eq))

  (global $return-sp (mut i32) (i32.const 0))
  ;; FIXME: Non-nullable.
  (table $return-stack 0 (ref null $kvarargs))

  ;; These grow functions try to grow their corresponding tables,
  ;; filling in a sensible default value so as to not require the
  ;; tables to be nullable, and abort if that fails.
  (func $grow-argv (param $diff i32)
    (br_if 0 (i32.ge_s (i32.const 0)
                       (table.grow $argv (ref.null i31)
                                         (local.get $diff))))
    (unreachable))
  (func $grow-return-stack (param $diff i32)
    (br_if 0 (i32.ge_s (i32.const 0)
                       (table.grow $return-stack (ref.null $kvarargs)
                                                 (local.get $diff))))
    (unreachable))

  (func $%make-struct1 (param (ref null $struct4) (ref eq))
        (result (ref $struct1))
    (struct.new $struct1 (i32.const 25) (local.get 0) (local.get 1)))
  (func $%make-struct2 (param (ref null $struct4) (ref eq) (ref eq))
        (result (ref $struct2))
    (struct.new $struct2 (i32.const 25) (local.get 0) (local.get 1) (local.get 2)))
  (func $%make-struct3 (param (ref null $struct4) (ref eq) (ref eq) (ref eq))
        (result (ref $struct3))
    (struct.new $struct3 (i32.const 25) (local.get 0) (local.get 1) (local.get 2) (local.get 3)))
  (func $%make-struct4 (param (ref null $struct4) (ref eq) (ref eq) (ref eq) (ref eq))
        (result (ref $struct4))
    (struct.new $struct4 (i32.const 25) (local.get 0)
                (local.get 1) (local.get 2) (local.get 3) (local.get 4)))
  (func $%make-simple-vtable
        (param $vt (ref null $struct4)) (param $flags i32) (param $nfields i32)
        (result (ref $struct4))
    (call $%make-struct4
          (local.get $vt)
          ;; field 0: flags: fixnum
          (i31.new (i32.shl (local.get $flags) (i32.const 1)))
          ;; field 1: nfields: fixnum
          (i31.new (i32.shl (local.get $nfields) (i32.const 1)))                
          ;; field 2: name: #f
          (i31.new (i32.const 1))
          ;; field 3: print: #f
          (i31.new (i32.const 1))))

  (global $root-vtable (mut (ref null $struct4)) (ref.null $struct4))
  
  (func $%init-structs
    (global.set $root-vtable
                (call $%make-simple-vtable
                      (ref.null $struct4)
                      (i32.const 3) ;; flags: validated | vtable-vtable
                      (i32.const 4))) ;; 4 fields
    ;; Tie the knot.
    (struct.set $struct4 1 (global.get $root-vtable) (global.get $root-vtable)))

  ;; When the module is instantiated, grow a new default-sized $argv and
  ;; $return-stack.
  (func $start
    (call $grow-argv (i32.const 42))
    (call $grow-return-stack (i32.const 42))
    (call $%init-structs)
    (call $%init-keywords)
    (call $%init-ports))
  (start $start)

  (func $set_arg (export "set_arg") (param $idx i32) (param $arg (ref eq))
    (table.set $argv (local.get $idx) (local.get $arg)))
  (func $get_arg (export "get_arg") (param $idx i32) (result (ref eq))
    (ref.as_non_null (table.get $argv (local.get $idx))))

  (func $describe (export "describe") (param $scm (ref eq)) (result (ref string))
    (local $tmp i32)
    (block $Ldone (result (ref string))
      ;; Verrrrry annoyingly, binaryen doesn't support all of WAT and
      ;; specifically doesn't do implicit stack arguments, so we can't
      ;; use br_on_cast as we would like.
      (block $Limmediate
        (block $Lheap-object
          (br_if $Limmediate (ref.test i31 (local.get $scm)))
          (br_if $Lheap-object (ref.test $heap-object (local.get $scm)))
          (unreachable))
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
                                    (block $Lprocedure
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
                                                            (block $Lunknown-tag
                                                              (br_table
                                                               $Lflonum
                                                               $Lbignum
                                                               $Lcomplex
                                                               $Lfraction
                                                               $Lpair
                                                               $Lmutable-pair
                                                               $Lvector
                                                               $Lmutable-vector
                                                               $Lbytevector
                                                               $Lmutable-bytevector
                                                               $Lbitvector
                                                               $Lmutable-bitvector
                                                               $Lstring
                                                               $Lmutable-string
                                                               $Lprocedure
                                                               $Lsymbol
                                                               $Lkeyword
                                                               $Lvariable
                                                               $Latomic-box
                                                               $Lhash-table
                                                               $Lweak-table
                                                               $Lfluid
                                                               $Ldynamic-state
                                                               $Lsyntax
                                                               $Lport
                                                               $Lstruct
                                                               $Lunknown-tag
                                                               (i32.and
                                                                (i32.const 31)
                                                                (struct.get $heap-object 0
                                                                            (ref.cast $heap-object
                                                                                      (local.get $scm))))))
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
                      (br_if $Lchar (i32.eq (i32.and (local.get $tmp) (i32.const 2))
                                            (i32.const 2)))
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

  (func $bignum-from-i64 (import "rt" "bignum_from_i64")
        (param i64) (result (ref extern)))
  (func $bignum-from-u64 (import "rt" "bignum_from_u64")
        (param i64) (result (ref extern)))
  (func $bignum-is-i64 (import "rt" "bignum_is_i64")
        (param (ref extern)) (result i32))
  (func $bignum-is-u64 (import "rt" "bignum_is_u64")
        (param (ref extern)) (result i32))
  (func $bignum-get-i64 (import "rt" "bignum_get_i64")
        (param (ref extern)) (result i64))
  (func $make-weak-map (import "rt" "make_weak_map")
        (result (ref extern)))
  (func $weak-map-get (import "rt" "weak_map_get")
        (param (ref extern)) (result (ref null eq)))
  (func $weak-map-set (import "rt" "weak_map_set")
        (param (ref extern) (ref eq) (ref eq)))
  (func $weak-map-delete (import "rt" "weak_map_delete")
        (param (ref extern) (ref eq)) (result i32))

  (func $scm-from-f64 (export "scm_from_f64") (param $v f64) (result (ref $flonum))
    (struct.new $flonum (i32.const 0) (local.get $v)))
  (func $scm-from-integer (export "scm_from_integer") (param $v (ref extern)) (result (ref eq))
    (local $tmp i64)
    (block $Lbignum
      (br_if $Lbignum (i32.eqz (call $bignum-is-i64 (local.get $v))))
      (local.set $tmp (call $bignum-get-i64 (local.get $v)))
      (br_if $Lbignum (i64.gt_u (i64.add (i64.const 0x2000_0000)
                                         (local.get $tmp))
                                (i64.const 0x4000_0000)))
      (br 1 (i31.new (i32.wrap_i64 (local.get $tmp)))))
    (struct.new $extern-ref (i32.const 1) (local.get $v)))
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
    (struct.new $fraction (i32.const 3) (local.get 0) (local.get 1)))
  (func $scm-from-complex (export "scm_from_complex") (param f64 f64) (result (ref $complex))
    ;; FIXME: check types?
    (struct.new $complex (i32.const 2) (local.get 0) (local.get 1)))
  (func $scm-from-string (export "scm_from_string") (param $str (ref string)) (result (ref $string))
    (struct.new $string (i32.const 12) (local.get $str)))

  (func $fixnum_value (export "fixnum_value") (param $v (ref i31)) (result i32)
    (i32.shr_s (i31.get_s (local.get $v)) (i32.const 1)))
  (func $char_value (export "char_value") (param $v (ref i31)) (result i32)
    (i32.shr_u (i31.get_s (local.get $v)) (i32.const 2)))
  (func $bignum_value (export "bignum_value") (param $v (ref $extern-ref)) (result (ref extern))
    (struct.get $extern-ref 1 (local.get $v)))
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
  (func $pair_car (export "car") (param $v (ref $pair)) (result (ref eq))
    (struct.get $pair 1 (local.get $v)))
  (func $pair_cdr (export "cdr") (param $v (ref $pair)) (result (ref eq))
    (struct.get $pair 2 (local.get $v)))

  (func $push-return (param $k (ref $kvarargs))
    (table.set $return-stack (global.get $return-sp) (local.get $k))
    (global.set $return-sp (i32.add (global.get $return-sp) (i32.const 1))))
  (func $pop-return (result (ref $kvarargs))
    (global.set $return-sp (i32.sub (global.get $return-sp) (i32.const 1)))
    (ref.as_non_null (table.get $return-stack (global.get $return-sp))))

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
                           (i32.const 0x27d4eb2d)))
    (i32.xor (local.get $v)
             (i32.shr_u (local.get $v) (i32.const 15))))

  (global $hashq-counter (mut i32) (i32.const 0))
  (func $%hashq (param $v (ref eq)) (result i32)
    (local $obj (ref $heap-object))
    (local $tag i32)
    (block $Limmediate
      (br_if $Limmediate (ref.test i31 (local.get $v)))
      (local.set $obj (ref.cast $heap-object (local.get $v)))
      (local.set $tag (struct.get $heap-object 0 (local.get $obj)))
      (block $Linitialized
        (br_if $Linitialized
               (i32.ne (i32.shr_u (local.get $tag) (i32.const 5))
                       (i32.const 0)))
        (global.set $hashq-counter
                    (i32.sub (global.get $hashq-counter) (i32.const 1)))
        (local.set $tag
                   (i32.or
                    (i32.or
                     (i32.const 32)
                     (i32.shl (call $integer-hash (global.get $hashq-counter))
                              (i32.const 6)))
                    (local.get $tag)))
        (struct.set $heap-object 0 (local.get $obj) (local.get $tag)))
      (br 1 (call $integer-hash (local.get $tag))))
    (call $integer-hash (i31.get_u (ref.cast i31 (local.get $v)))))

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

  (type $symtab-entry
        (struct (field $sym (ref $symbol)) (field $next (ref null $symtab-entry))))
  (type $symtab (array (mut (ref null $symtab-entry))))
  (global $the-symtab (ref $symtab)
          (array.new $symtab (ref.null $symtab-entry) (i32.const 47)))
  (func $string-to-symbol (param $str (ref string)) (result (ref $symbol))
    (local $hash i32)
    (local $idx i32)
    (local $tag i32)
    (local $entry (ref null $symtab-entry))
    (local $ret (ref null $symbol))
    (local.set $hash (call $string-hash (local.get $str)))
    (local.set $idx (i32.rem_u (local.get $hash) (array.len (global.get $the-symtab))))
    ;; Attach symbol tag to hash.
    (local.set $tag (i32.or (i32.shl (local.get $hash) (i32.const 6))
                            (i32.const 47))) ;; #b101111
    (local.set $entry (array.get $symtab (global.get $the-symtab) (local.get $idx)))
    (block $done
      (block $insert
        (loop $lp
          (br_if $insert (ref.is_null (local.get $entry)))
          (block $next
            (br_if $next
                   (i32.ne (struct.get $symbol 0
                                       (struct.get $symtab-entry 0 (local.get $entry)))
                           (local.get $tag)))
            (br_if $next
                   (i32.eqz
                    (string.eq (struct.get $string 1
                                           (struct.get $symbol 1
                                                       (struct.get $symtab-entry 0
                                                                   (local.get $entry))))
                               (local.get $str))))
            (local.set $ret (struct.get $symtab-entry 0 (local.get $entry)))
            (br $done))
          (local.set $entry (struct.get $symtab-entry 1 (local.get $entry)))
          (br $lp)))
      (local.set $ret (struct.new $symbol (local.get $tag)
                                  ;; Immutable string.
                                  (struct.new $string (i32.const 12) (local.get $str))))
      (local.set $entry (array.get $symtab (global.get $the-symtab) (local.get $idx)))
      (array.set $symtab (global.get $the-symtab) (local.get $idx)
                 (struct.new $symtab-entry (ref.as_non_null (local.get $ret))
                             (local.get $entry))))
    (ref.as_non_null (local.get $ret)))

  (func $%make-hash-table (result (ref $hash-table))
    (struct.new $hash-table (i32.const 19) (i31.new (i32.const 0))
                (struct.new $vector (i32.const 7)
                            (array.new $raw-scmvector
                                       (i31.new (i32.const 13)) (i32.const 47)))))
  (func $%hashq-lookup (param $tab (ref $hash-table)) (param $k (ref eq))
        (result (ref null $pair))
    (local $idx i32)
    (local $buckets (ref $raw-scmvector))
    (local $chain (ref eq))
    (local $head (ref $pair))
    (local.set $buckets (struct.get $vector 1 (struct.get $hash-table 2 (local.get $tab))))
    (local.set $idx (i32.rem_u (call $%hashq (local.get $k))
                               (array.len (local.get $buckets))))
    (local.set $chain (array.get $raw-scmvector (local.get $buckets) (local.get $idx)))
    (block $not-found
      (loop $lp
        (br_if $not-found (i32.eqz (ref.test $pair (local.get $chain))))
        (local.set $head
                   (ref.cast $pair (struct.get $pair 1
                                               (ref.cast $pair (local.get $chain)))))
        (local.set $chain (struct.get $pair 2 (ref.cast $pair (local.get $chain))))
        (block $found
          (br_if $found
                 (ref.eq (struct.get $pair 1 (local.get $head))
                         (local.get $k)))
          (br $lp))
        (br 2 (local.get $head))))
    (ref.null $pair))
  (func $%hashq-insert (param $tab (ref $hash-table)) (param $k (ref eq)) (param $v (ref eq))
    (local $idx i32)
    (local $buckets (ref $raw-scmvector))
    (local.set $buckets (struct.get $vector 1 (struct.get $hash-table 2 (local.get $tab))))
    (local.set $idx (i32.rem_u (call $%hashq (local.get $k))
                               (array.len (local.get $buckets))))
    (array.set $raw-scmvector
               (local.get $buckets) (local.get $idx)
               (struct.new $pair (i32.const 5)
                           (struct.new $pair (i32.const 5) (local.get $k) (local.get $v))
                           (array.get $raw-scmvector (local.get $buckets) (local.get $idx))))
    (struct.set $hash-table 1
                (local.get $tab)
                (i31.new (i32.add (i32.const 1)
                                  (i31.get_s (struct.get $hash-table 1 (local.get $tab)))))))

  (global $the-kwtab (mut (ref null $hash-table)) (ref.null $hash-table))
  (func $%init-keywords
    (global.set $the-kwtab (call $%make-hash-table)))
  (func $symbol-to-keyword (param $sym (ref $symbol)) (result (ref $keyword))
    (local $entry (ref null $pair))
    (local $new-kw (ref $keyword))
    (local.set $entry (call $%hashq-lookup
                            (ref.as_non_null (global.get $the-kwtab))
                            (local.get $sym)))
    (block $not-found
      (br_if $not-found (ref.is_null (local.get $entry)))
      (br 1 (ref.cast $keyword (struct.get $pair 2 (local.get $entry)))))
    (local.set $new-kw
               (struct.new $keyword
                           ;; Symbol tag is 15, kw is 16.  Just add 1 to tag.
                           (i32.add (struct.get $symbol 0 (local.get $sym))
                                    (i32.const 1))
                           (local.get $sym)))
    (call $%hashq-insert
          (ref.as_non_null (global.get $the-kwtab))
          (local.get $sym)
          (local.get $new-kw))
    (local.get $new-kw))

  ;; Things like this should be implemented in Scheme.
  (type $string-input-port-stream
        (struct (field $bv (ref $raw-bytevector))
                (field $pos (mut i32))))
  (func $string-input-port-read (param $nargs i32) (result i32)
    (local $port (ref $port))
    (local $dst (ref $raw-bytevector))
    (local $start i32)
    (local $count i32)
    (local $stream (ref $string-input-port-stream))
    (local $src (ref $raw-bytevector))
    (local $pos i32)
    (local $avail i32)
    (local $i i32)
    (block $check-nargs
      (br_if $check-nargs (i32.eq (local.get $nargs) (i32.const 4)))
      (unreachable))
    (local.set $port (ref.cast $port (table.get $argv (i32.const 0))))
    (local.set $dst (struct.get $bytevector 1
                                (ref.cast $bytevector (table.get $argv (i32.const 1)))))
    (local.set $start (i31.get_s (ref.cast i31 (table.get $argv (i32.const 2)))))
    (local.set $count (i31.get_s (ref.cast i31 (table.get $argv (i32.const 3)))))
    (local.set $stream (ref.cast $string-input-port-stream
                                 (struct.get $port $stream (local.get $port))))
    (local.set $src (struct.get $string-input-port-stream 0 (local.get $stream)))
    (local.set $pos (struct.get $string-input-port-stream 1 (local.get $stream)))
    (local.set $avail
               (i32.sub (array.len (local.get $src)) (local.get $pos)))
    (block $trim
      (br_if $trim (i32.lt_s (local.get $count) (local.get $avail)))
      (local.set $count (local.get $avail)))
    (block $done
      (loop $lp
        (br_if $done (i32.eq (local.get $i) (local.get $count)))
        (array.set $raw-bytevector
                   (local.get $dst)
                   (i32.add (local.get $start) (local.get $i))
                   (array.get_u $raw-bytevector (local.get $src)
                                (i32.add (local.get $pos) (local.get $i))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $lp)))
    (struct.set $string-input-port-stream 1 (local.get $stream)
                (i32.add (local.get $pos) (local.get $count)))
    (table.set $argv (i32.const 0) (i31.new (local.get $count)))
    (return_call_ref $kvarargs (i32.const 1) (call $pop-return)))
  (func $string-input-port-seek (param $nargs i32) (result i32)
    ;; Not yet implemented.
    (unreachable))
  (func $string-input-port-random-access? (param $nargs i32) (result i32)
    ;; Not yet implemented.
    (unreachable))
  (func $string-input-port-input-waiting? (param $nargs i32) (result i32)
    ;; Not yet implemented.
    (unreachable))

  (func $string-output-port-write (param $nargs i32) (result i32)
    ;; Not yet implemented.
    (unreachable))

  (global $string-input-port-type (mut (ref null $port-type)) (ref.null $port-type))
  (global $string-output-port-type (mut (ref null $port-type)) (ref.null $port-type))
  (func $%init-ports
    (global.set
     $string-input-port-type
     (struct.new
      $port-type
      (string.const "string-input-port")
      (struct.new $proc (i32.const 14) (ref.func $string-input-port-read))
      (ref.null $proc) ;; write
      (struct.new $proc (i32.const 14) (ref.func $string-input-port-seek))
      (ref.null $proc) ;; close
      (ref.null $proc) ;; get-natural-buffer-sizes
      (struct.new $proc (i32.const 14) (ref.func $string-input-port-random-access?))
      (struct.new $proc (i32.const 14) (ref.func $string-input-port-input-waiting?))
      (ref.null $proc) ;; truncate
      ))
    (global.set
     $string-output-port-type
     (struct.new
      $port-type
      (string.const "string-output-port")
      (ref.null $proc) ;; read
      (struct.new $proc (i32.const 14) (ref.func $string-output-port-write)) ;; write
      (ref.null $proc) ;; seek
      (ref.null $proc) ;; close
      (ref.null $proc) ;; get-natural-buffer-sizes
      (ref.null $proc) ;; random-access?
      (ref.null $proc) ;; input-waiting?
      (ref.null $proc) ;; truncate
      )))
  (func $%make-string-input-port (param $str (ref string)) (result (ref $port))
    (local $wtf8 (ref $raw-bytevector))
    ;; FIXME!!!!! Binaryen borks measure_utf8 and measure_wtf8
    (local.set $wtf8
               (array.new_default $raw-bytevector (string.measure_wtf16 (local.get $str))))
    ;; FIXME!!! binaryen borks this too
    ;; (string.encode_lossy_utf8_array (local.get $str) (local.get $wtf8) (i32.const 0))
    (struct.new $port (i32.const 24)
                (ref.cast $port-type (global.get $string-input-port-type))
                (struct.new $string-input-port-stream
                            (local.get $wtf8)
                            (i32.const 0))
                (i31.new (i32.const 1)) ;; file_name
                ;; position: (cons 0 0)
                (struct.new $pair (i32.const 5)
                            (i31.new (i32.const 0)) (i31.new (i32.const 0)))
                (i31.new (i32.const 1)) ;; read buf: #f
                (i31.new (i32.const 1)) ;; write buf: #f
                (i31.new (i32.const 1)) ;; write buf aux: #f
                (i32.const 0) ;; read-buffering
                (i32.const 0) ;; refcount
                (i32.const 0) ;; rw_random ?
                (i31.new (i32.const 13)) ;; properties: ()
                ))

  (func $main (param $nargs i32) (result i32)
    ;; Fixnum: 1.
    (table.set $argv (i32.const 0) (i31.new (i32.const 2)))
    ;; Char: 1.
    (table.set $argv (i32.const 1) (i31.new (i32.const 7)))
    ;; False.
    (table.set $argv (i32.const 2) (i31.new (i32.const 1)))
    ;; Null.
    (table.set $argv (i32.const 3) (i31.new (i32.const 13)))
    ;; True.
    (table.set $argv (i32.const 4) (i31.new (i32.const 17)))
    ;; Unspecified.
    (table.set $argv (i32.const 5) (i31.new (i32.const 33)))
    ;; EOF.
    (table.set $argv (i32.const 6) (i31.new (i32.const 41)))
    ;; '(1 . 2)
    (table.set $argv (i32.const 7)
               (struct.new $pair (i32.const 4)
                           (i31.new (i32.const 2)) (i31.new (i32.const 4))))
    ;; (cons 1 2)
    (table.set $argv (i32.const 8)
               (struct.new $pair (i32.const 5)
                           (i31.new (i32.const 2)) (i31.new (i32.const 4))))
    ;; #(#f #f #f)
    (table.set $argv (i32.const 9)
               (struct.new $vector (i32.const 6)
                           (array.new $raw-scmvector (i31.new (i32.const 1)) (i32.const 3))))
    ;; (vector #f #f #f)
    (table.set $argv (i32.const 10)
               (struct.new $vector (i32.const 7)
                           (array.new $raw-scmvector (i31.new (i32.const 1)) (i32.const 3))))
    ;; #vu8(0 0 0 0 0)
    (table.set $argv (i32.const 11)
               (struct.new $bytevector (i32.const 8)
                           (array.new_default $raw-bytevector (i32.const 5))))
    ;; (bytevector 0 0 0 0 0)
    (table.set $argv (i32.const 12)
               (struct.new $bytevector (i32.const 9)
                           (array.new_default $raw-bytevector (i32.const 5))))
    ;; #*11111
    (table.set $argv (i32.const 13)
               (struct.new $bitvector (i32.const 10)
                           (i32.const 5)
                           (array.new $raw-bitvector (i32.const 31) (i32.const 1))))
    ;; (bitvector #t #t #t #t #t)
    (table.set $argv (i32.const 14)
               (struct.new $bitvector (i32.const 11)
                           (i32.const 5)
                           (array.new $raw-bitvector (i32.const 31) (i32.const 1))))
    ;; "hello world!"
    (table.set $argv (i32.const 15)
               (struct.new $string (i32.const 12)
                           (string.const "hello world!")))
    ;; (string #\h #\e #\l #\l #\o #\o)
    (table.set $argv (i32.const 16)
               (struct.new $string (i32.const 13)
                           (string.const "helloo")))
    ;; #<procedure main>
    (table.set $argv (i32.const 17)
               (struct.new $proc (i32.const 14) (ref.func $main)))
    ;; 'my-symbol
    (table.set $argv (i32.const 18)
               (call $string-to-symbol (string.const "my-symbol")))
    ;; #:my-symbol
    (table.set $argv (i32.const 19)
               (call $symbol-to-keyword
                     (call $string-to-symbol (string.const "my-symbol"))))
    ;; (make-variable #f)
    (table.set $argv (i32.const 20)
               (struct.new $box (i32.const 17) (i31.new (i32.const 1))))
    ;; (make-atomic-box #f)
    (table.set $argv (i32.const 21)
               (struct.new $box (i32.const 18) (i31.new (i32.const 1))))
    ;; (make-hash-table)
    (table.set $argv (i32.const 22)
               (call $%make-hash-table))
    ;; (make-weak-key-hash-table)
    (table.set $argv (i32.const 23)
               (struct.new $extern-ref (i32.const 20)
                           (call $make-weak-map)))
    ;; (make-struct (make-vtable 1) #f)
    (table.set $argv (i32.const 24)
               (call $%make-struct1
                     (call $%make-simple-vtable
                           (global.get $root-vtable)
                           ;; flags: validated
                           (i32.const 1)
                           ;; one field
                           (i32.const 1))
                     (i31.new (i32.const 1))))
    ;; 42.69
    (table.set $argv (i32.const 25)
               (struct.new $flonum (i32.const 0) (f64.const 42.69)))
    ;; 42.69
    (table.set $argv (i32.const 26)
               (struct.new $extern-ref (i32.const 1)
                           (call $bignum-from-i64 (i64.const 42))))
    ;; 42+6.9i
    (table.set $argv (i32.const 27)
               (struct.new $complex (i32.const 2)
                           (f64.const 42) (f64.const 6.9)))
    ;; 14/23
    (table.set $argv (i32.const 28)
               (struct.new $fraction (i32.const 3)
                           (i31.new (i32.const 28))
                           (i31.new (i32.const 46))))
    ;; (make-fluid #f)
    (table.set $argv (i32.const 29)
               (struct.new $fluid (i32.const 21) (i31.new (i32.const 1))))
    ;; (current-dynamic-state)
    (table.set $argv (i32.const 30)
               (struct.new $extern-ref (i32.const 22)
                           (call $make-weak-map)))
    ;; (datum->syntax #f '() #:source #f)
    (table.set $argv (i32.const 31)
               (struct.new $syntax (i32.const 23)
                           (i31.new (i32.const 13)) ;; datum: ()
                           (struct.new $pair (i32.const 4)
                                       (i31.new (i32.const 13))
                                       (i31.new (i32.const 13))) ;; wrap: empty-wrap: (())
                           (i31.new (i32.const 1)) ;; module: #f
                           (i31.new (i32.const 1)) ;; source: #f
                           ))
    ;; (current-input-port)
    (table.set $argv (i32.const 32)
               (call $%make-string-input-port (string.const "hey!!!")))
    ;; Remaining data types: port
    (return_call_ref $kvarargs (i32.const 33) (call $pop-return)))

  (func $return (param $nargs i32) (result i32)
    (local.get $nargs))

  (func $init (export "_init") (param $nargs i32) (result i32)
    (call $push-return (ref.func $return))
    (return_call $main (local.get $nargs))))
