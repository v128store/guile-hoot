(module
  (type $kvarargs (func (param i32) (result i32)))

  (type $raw-immutable-bitvector (array i32))
  (type $raw-immutable-bytevector (array i8))

  (type $raw-bitvector (array (mut i32)))
  (type $raw-bytevector (array (mut i8)))
  (type $vector (array (mut (ref eq))))

  (type $void-struct (struct))
  (type $heap-object (sub $void-struct (struct)))

  (type $pair
        (sub $heap-object
             (struct (field $car (mut (ref eq))) (field $cdr (mut (ref eq))))))
  (type $immutable-pair
        (sub $heap-object
             (struct (field $car (ref eq)) (field $cdr (ref eq)))))
  ;; Vtable link is mutable so that we can tie the knot for top types.
  (type $struct
        (sub $heap-object
             (struct
              (field $vtable (mut (ref null $struct)))
              (field $fields (ref $vector)))))
  ;; Built-in support for closures is a post-MVP feature.
  (type $proc
    (sub $heap-object
         (struct (field $func (ref $kvarargs))
                 (field $free-vars (ref null $vector)))))
  (type $string
    (sub $heap-object (struct (field $str (ref string)))))
  (type $symbol
    (sub $heap-object (struct (field $hash i32) (field $name (ref string)))))
  (type $keyword
    (sub $heap-object
         (struct (field $name (ref $symbol)))))
  (type $box
    (sub $heap-object
         ;; Box kind: 0: variable, 1: atomic box
         (struct (field $kind i8)
                 (field $val (mut (ref eq))))))
  (type $extern-ref
    (sub $heap-object
         ;; Kind: 0: hash-table, 1: dynamic-state
         (struct (field $kind i8)
                 (field $val (ref extern)))))
  (type $fluid
    (sub $heap-object
         (struct (field $hash i32) (field $init (ref eq)))))
  (type $syntax
    (sub $heap-object
         (struct
          (field $expr (ref eq))
          (field $wrap (ref eq))
          (field $module (ref eq))
          (field $source (ref eq)))))
  (type $bitvector
    (sub $heap-object
         (struct
          (field $len i32)
          (field $bits (ref $raw-bitvector)))))
  (type $port-type
    (struct
      (field $name (ref eq))
      ;; in guile these are (port, bv, start, count) -> size_t
      (field $read (ref $closure)) ;; could have a more refined type
      (field $write (ref $closure))
      (field $seek (ref $closure)) ;; (port, offset, whence) -> offset
      (field $close (ref $closure)) ;; (port) -> ()
      (field $get-natural-buffer-sizes (ref $closure)) ;; port -> (rdsz, wrsz)
      (field $random-access? (ref $closure)) ;; port -> bool
      (field $input-waiting (ref $closure)) ;; port -> bool
      (field $truncate (ref $closure)) ;; (port, length) -> ()
      (field $flags i32)
      ;; Guile also has GOOPS classes here.
      ))
  (type $port
    (sub $heap-object
         (struct
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
  (type $heap-number (sub $heap-object (struct)))
  (type $bignum
    (sub $heap-number
         (struct (field $val (ref extern))))) ;; BigInt
  (type $flonum
    (sub $heap-number
         (struct (field $val f64))))
  (type $complex
    (sub $heap-number
         (struct (field $real f64) (field $imag f64))))
  (type $fraction
    (sub $heap-number
         (struct (field $num (ref eq)) (field $denom (ref eq)))))

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

  (func $%make-vtable
        (param $vt (ref null $struct)) (param $flags i32) (param $nfields i32)
        (result (ref $struct))
    (local $fields (ref $vector))
    ;; fixme: pull nfields from vtable
    (local.set $fields
               (array.new $vector (i31.new (i32.const 1))
                          (i32.const 4)))
    ;; field 0: flags: fixnum(validated | vtable-vtable)
    (array.set $vector
               (local.get $fields)
               (i32.const 0)
               (i31.new (i32.shl (local.get $flags) (i32.const 1))))
    ;; field 1: nfields: fixnum(6)
    (array.set $vector
               (local.get $fields)
               (i32.const 1)
               (i31.new (i32.shl (local.get $nfields) (i32.const 1))))
    ;; field 2: name: leave as #f
    ;; field 3: print: leave as #f
    (struct.new $struct (local.get $vt) (local.get $fields)))
  (func $%make-struct (param $vt (ref $struct)) (result (ref $struct))
    (struct.new $struct
                (local.get $vt)
                (array.new $vector
                           ;; Init fields to #f.
                           (i31.new (i32.const 1))
                           (i32.shr_u
                            (i31.get_s
                             (ref.cast
                              i31
                              (array.get $vector
                                         (struct.get $struct 1 (local.get $vt))
                                         (i32.const 1))))
                            (i32.const 1)))))

  (global $root-vtable (mut (ref null $struct)) (ref.null $struct))
  
  ;; When the module is instantiated, grow a new default-sized $argv and
  ;; $return-stack.
  (func $start
    (call $grow-argv (i32.const 42))
    (call $grow-return-stack (i32.const 42))
    (global.set $root-vtable
                (call $%make-vtable
                      (ref.null $struct)
                      ;; flags: validated | vtable-vtable
                      (i32.const 3)
                      ;; 4 fields
                      (i32.const 4)))
    ;; Tie the knot.
    (struct.set $struct 0 (global.get $root-vtable) (global.get $root-vtable)))
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
        (block $Limmutable-pair
          (block $Lpair
            (block $Lbytevector
              (block $Limmutable-bytevector
                (block $Lstruct
                  (block $Lprocedure
                    (block $Lstring
                      (block $Lsymbol
                        (block $Lkeyword
                          (block $Lbox
                            (block $Lvector
                              (block $Lextern-ref
                                (block $Lfluid
                                  (block $Lsyntax
                                    (block $Lbitvector
                                      (block $Lport
                                        (block $Lflonum
                                          (block $Lbignum
                                            (block $Lcomplex
                                              (block $Lfraction
                                                (br_if $Limmediate
                                                       (ref.test i31 (local.get $scm)))
                                                (br_if $Limmutable-pair
                                                       (ref.test $immutable-pair (local.get $scm)))
                                                (br_if $Lpair
                                                       (ref.test $pair (local.get $scm)))
                                                (br_if $Lbytevector
                                                       (ref.test $raw-bytevector (local.get $scm)))
                                                (br_if $Limmutable-bytevector
                                                       (ref.test $raw-immutable-bytevector (local.get $scm)))
                                                (br_if $Lstruct
                                                       (ref.test $struct (local.get $scm)))
                                                (br_if $Lprocedure
                                                       (ref.test $proc (local.get $scm)))
                                                (br_if $Lstring
                                                       (ref.test $string (local.get $scm)))
                                                (br_if $Lsymbol
                                                       (ref.test $symbol (local.get $scm)))
                                                (br_if $Lkeyword
                                                       (ref.test $keyword (local.get $scm)))
                                                (br_if $Lbox
                                                       (ref.test $box (local.get $scm)))
                                                (br_if $Lvector
                                                       (ref.test $vector (local.get $scm)))
                                                (br_if $Lextern-ref
                                                       (ref.test $extern-ref (local.get $scm)))
                                                (br_if $Lfluid
                                                       (ref.test $fluid (local.get $scm)))
                                                (br_if $Lsyntax
                                                       (ref.test $syntax (local.get $scm)))
                                                (br_if $Lbitvector
                                                       (ref.test $bitvector (local.get $scm)))
                                                (br_if $Lport
                                                       (ref.test $port (local.get $scm)))
                                                (br_if $Lflonum
                                                       (ref.test $flonum (local.get $scm)))
                                                (br_if $Lbignum
                                                       (ref.test $bignum (local.get $scm)))
                                                (br_if $Lcomplex
                                                       (ref.test $complex (local.get $scm)))
                                                (br_if $Lfraction
                                                       (ref.test $fraction (local.get $scm)))
                                                (unreachable))
                                              (br $Ldone (string.const "fraction")))
                                            (br $Ldone (string.const "complex")))
                                          (br $Ldone (string.const "bignum")))
                                        (br $Ldone (string.const "flonum")))
                                      (br $Ldone (string.const "port")))
                                    (br $Ldone (string.const "bitvector")))
                                  (br $Ldone (string.const "syntax")))
                                (br $Ldone (string.const "fluid")))
                              (block $Lhash-table
                                (block $Ldynamic-state
                                  (block $Lunknown-extern-ref
                                    (br_table
                                     $Lhash-table $Ldynamic-state $Lunknown-extern-ref
                                     (struct.get_u $extern-ref 0
                                                   (ref.cast $extern-ref (local.get $scm)))))
                                  (unreachable))
                                (br $Ldone (string.const "dynamic-state")))
                              (br $Ldone (string.const "hash-table")))
                            (br $Ldone (string.const "vector")))
                          (block $Lvariable
                            (block $Latomic-box
                              (block $Lunknown-box
                                (br_table
                                 $Lvariable $Latomic-box $Lunknown-box
                                 (struct.get_u $box 0
                                               (ref.cast $box (local.get $scm)))))
                              (unreachable))
                            (br $Ldone (string.const "atomic-box")))
                          (br $Ldone (string.const "variable")))
                        (br $Ldone (string.const "keyword")))
                      (br $Ldone (string.const "symbol")))
                    (br $Ldone (string.const "string")))
                  (br $Ldone (string.const "procedure")))
                (br $Ldone (string.const "struct")))
              (br $Ldone (string.const "immutable-bytevector")))
            ;; Bytevector.
            (br $Ldone (string.const "bytevector")))
          (br $Ldone (string.const "pair")))
        (br $Ldone (string.const "immutable-pair")))
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

  (func $bignum-is-i64 (import "rt" "bignum_is_i64")
        (param (ref extern)) (result i32))
  (func $bignum-is-u64 (import "rt" "bignum_is_u64")
        (param (ref extern)) (result i32))
  (func $bignum-get-i64 (import "rt" "bignum_get_i64")
        (param (ref extern)) (result i64))

  (func $scm-from-f64 (export "scm_from_f64") (param $v f64) (result (ref $flonum))
    (struct.new $flonum (local.get $v)))
  (func $scm-from-integer (export "scm_from_integer") (param $v (ref extern)) (result (ref eq))
    (local $tmp i64)
    (block $Lbignum
      (br_if $Lbignum (i32.eqz (call $bignum-is-i64 (local.get $v))))
      (local.set $tmp (call $bignum-get-i64 (local.get $v)))
      (br_if $Lbignum (i64.gt_u (i64.add (i64.const 0x2000_0000)
                                         (local.get $tmp))
                                (i64.const 0x4000_0000)))
      (br 1 (i31.new (i32.wrap_i64 (local.get $tmp)))))
    (struct.new $bignum (local.get $v)))
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
  (func $scm-from-string (export "scm_from_string") (param $str (ref string)) (result (ref $string))
    (struct.new $string (local.get $str)))

  (func $fixnum_value (export "fixnum_value") (param $v (ref i31)) (result i32)
    (i32.shr_s (i31.get_s (local.get $v)) (i32.const 1)))
  (func $char_value (export "char_value") (param $v (ref i31)) (result i32)
    (i32.shr_u (i31.get_s (local.get $v)) (i32.const 2)))
  (func $bignum_value (export "bignum_value") (param $v (ref $bignum)) (result (ref extern))
    (struct.get $bignum 0 (local.get $v)))
  (func $flonum_value (export "flonum_value") (param $v (ref $flonum)) (result f64)
    (struct.get $flonum 0 (local.get $v)))
  (func $string_value (export "string_value") (param $v (ref $string)) (result (ref string))
    (struct.get $string 0 (local.get $v)))
  (func $pair_car (export "car") (param $v (ref $immutable-pair)) (result (ref eq))
    (struct.get $immutable-pair 0 (local.get $v)))
  (func $pair_cdr (export "cdr") (param $v (ref $immutable-pair)) (result (ref eq))
    (struct.get $immutable-pair 1 (local.get $v)))

  (func $push-return (param $k (ref $kvarargs))
    (table.set $return-stack (global.get $return-sp) (local.get $k))
    (global.set $return-sp (i32.add (global.get $return-sp) (i32.const 1))))
  (func $pop-return (result (ref $kvarargs))
    (global.set $return-sp (i32.sub (global.get $return-sp) (i32.const 1)))
    (ref.as_non_null (table.get $return-stack (global.get $return-sp))))

  ;; For now, the Java string hash function, except over codepoints
  ;; rather than WTF-16 code units.
  (func $string_hash (param $str (ref string)) (result i32)
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
  (func $string_to_symbol (param $str (ref string)) (result (ref $symbol))
    (local $hash i32)
    (local $idx i32)
    (local $entry (ref null $symtab-entry))
    (local $ret (ref null $symbol))
    (local.set $hash (call $string_hash (local.get $str)))
    (local.set $idx (i32.rem_u (local.get $hash) (array.len (global.get $the-symtab))))
    (local.set $entry (array.get $symtab (global.get $the-symtab) (local.get $idx)))
    (block $done
      (block $insert
        (loop $lp
          (br_if $insert (ref.is_null (local.get $entry)))
          (block $next
            (br_if $next
                   (i32.ne (struct.get $symbol 0
                                       (struct.get $symtab-entry 0 (local.get $entry)))
                           (local.get $hash)))
            (br_if $next
                   (i32.eqz
                    (string.eq (struct.get $symbol 1
                                           (struct.get $symtab-entry 0 (local.get $entry)))
                               (local.get $str))))
            (local.set $ret (struct.get $symtab-entry 0 (local.get $entry)))
            (br $done))
          (local.set $entry (struct.get $symtab-entry 1 (local.get $entry)))
          (br $lp)))
      (local.set $ret (struct.new $symbol (local.get $hash) (local.get $str)))
      (local.set $entry (array.get $symtab (global.get $the-symtab) (local.get $idx)))
      (array.set $symtab (global.get $the-symtab) (local.get $idx)
                 (struct.new $symtab-entry (ref.as_non_null (local.get $ret))
                             (local.get $entry))))
    (ref.as_non_null (local.get $ret)))

  (type $kwtab-entry
        (struct (field $kw (ref $keyword)) (field $next (ref null $kwtab-entry))))
  (type $kwtab (array (mut (ref null $kwtab-entry))))
  (global $the-kwtab (ref $kwtab)
          (array.new $kwtab (ref.null $kwtab-entry) (i32.const 47)))
  (func $symbol_to_keyword (param $sym (ref $symbol)) (result (ref $keyword))
    (local $idx i32)
    (local $entry (ref null $kwtab-entry))
    (local $ret (ref null $keyword))
    (local.set $idx (i32.rem_u (struct.get $symbol 0 (local.get $sym))
                               (array.len (global.get $the-kwtab))))
    (local.set $entry (array.get $kwtab (global.get $the-kwtab) (local.get $idx)))
    (block $done
      (block $insert
        (loop $lp
          (br_if $insert (ref.is_null (local.get $entry)))
          (block $next
            (br_if $next
                   (i32.eqz
                    (ref.eq (struct.get $keyword 0
                                        (struct.get $kwtab-entry 0 (local.get $entry)))
                            (local.get $sym))))
            (local.set $ret (struct.get $kwtab-entry 0 (local.get $entry)))
            (br $done))
          (local.set $entry (struct.get $kwtab-entry 1 (local.get $entry)))
          (br $lp)))
      (local.set $ret (struct.new $keyword (local.get $sym)))
      (local.set $entry (array.get $kwtab (global.get $the-kwtab) (local.get $idx)))
      (array.set $kwtab (global.get $the-kwtab) (local.get $idx)
                 (struct.new $kwtab-entry (ref.as_non_null (local.get $ret))
                             (local.get $entry))))
    (ref.as_non_null (local.get $ret)))

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
    ;; (cons 1 2)
    (table.set $argv (i32.const 7)
               (struct.new $immutable-pair
                           (i31.new (i32.const 2)) (i31.new (i32.const 4))))
    ;; (mcons 1 2)
    (table.set $argv (i32.const 8)
               (struct.new $pair
                           (i31.new (i32.const 2)) (i31.new (i32.const 4))))
    (table.set $argv (i32.const 9)
               (array.new_default $raw-immutable-bytevector (i32.const 5)))
    (table.set $argv (i32.const 10)
               (array.new_default $raw-bytevector (i32.const 5)))

    (table.set $argv (i32.const 11)
               (call $%make-struct
                     (call $%make-vtable
                           (global.get $root-vtable)
                           ;; flags: validated
                           (i32.const 1)
                           ;; one field
                           (i32.const 1))))
    (table.set $argv (i32.const 12)
               (struct.new $proc (ref.func $main) (ref.null $vector)))
    (table.set $argv (i32.const 13)
               (struct.new $string (string.const "hello world!")))
    (table.set $argv (i32.const 14)
               (call $string_to_symbol (string.const "my-symbol")))
    (table.set $argv (i32.const 15)
               (call $symbol_to_keyword
                     (call $string_to_symbol (string.const "my-symbol"))))
    (table.set $argv (i32.const 16)
               (struct.new $box (i32.const 0) (i31.new (i32.const 1))))
    (table.set $argv (i32.const 17)
               (struct.new $box (i32.const 1) (i31.new (i32.const 1))))
    (table.set $argv (i32.const 18)
               (array.new $vector (i31.new (i32.const 1)) (i32.const 10)))
    ;;                           $Lvector
    ;;                           $Lhash-table
    ;;                           $Lfluid
    ;;                           $Ldynamic-state
    ;;                           $Lsyntax
    ;;                           $Lbitvector
    ;;                           $Lport
    ;;                           $Lheap-number
    ;;_if $Lflonum (i32.eq (local.get $tmp) (i32.const 15)))
    ;;_if $Lbignum (i32.eq (local.get $tmp) (i32.const 31)))
    ;;_if $Lcomplex (i32.eq (local.get $tmp) (i32.const 47)))
    ;;_if $Lfraction (i32.eq (local.get $tmp) (i32.const 63)))
    (return_call_ref $kvarargs (i32.const 19) (call $pop-return)))

  (func $return (param $nargs i32) (result i32)
    (local.get $nargs))
  (func $init (export "_init") (param $nargs i32) (result i32)
    (call $push-return (ref.func $return))
    (return_call $main (local.get $nargs))))
