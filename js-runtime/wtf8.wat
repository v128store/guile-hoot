(type $wtf8 (array (mut i8)))
(type $iter
      (struct (field $wtf8 (ref $wtf8))
              (field $pos (mut i32))))
(type $builder
      (struct (field $wtf8 (mut (ref $wtf8)))
              (field $pos (mut i32))))

(memory $decoder 1)

(data
 (memory $decoder)
 (i32.const 0)
 ;; Generalized UTF-8 decoder is a translation of:
 ;; https://chromium.googlesource.com/v8/v8/+/main/src/third_party/utf8-decoder/generalized-utf8-decoder.h
 ;;
 ;; Transitions:
 ;; This first table maps bytes to character to a transition.
 ;;
 ;; The transition value takes a state to a new state, but it also determines
 ;; the set of bits from the current byte that contribute to the decoded
 ;; codepoint:
 ;;
 ;;   Transition | Current byte bits that contribute to decoded codepoint
 ;;   -------------------------------------------------------------------
 ;;    0, 1      | #b01111111
 ;;    2, 3      | #b00111111
 ;;    4, 5      | #b00011111
 ;;    6, 7      | #b00001111
 ;;    8, 9      | #b00000111
 ;;    10        | #b00000011
 ;;
 ;; Given the WTF-8 encoding, we therefore have the following constraints:
 ;;
 ;;   1. The transition value for 1-byte encodings should have the value 0 or
 ;;      1 so that we preserve all of the low 7 bits.
 ;;   2. Continuation bytes (#x80 to #xBF) are of the form #b10xxxxxx, and
 ;;      therefore should have transition value between 0 and 3.
 ;;   3. Leading bytes for 2-byte encodings are of the form #b110yyyyy, and
 ;;      therefore the transition value can be between 2 and 5.
 ;;   4. Leading bytes for 3-byte encodings (#b1110zzzz) need transition
 ;;      value between 4 and 7.
 ;;   5. Leading bytes for 4-byte encodings (#b11110uuu) need transition
 ;;      value between 6 and 9.
 ;;   6. We need more states to impose irregular constraints.  Sometimes we
 ;;      can use the knowldege that e.g. some high significant bits of the
 ;;      xxxx in #b1110xxxx are 0, then we can use a higher transition value.
 ;;   7. Transitions to invalid states can use any transition value.
 #vu8(0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0   ;; 00-0F
      0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 ;; 10-1F
      0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 ;; 20-2F
      0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 ;; 30-3F
      0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 ;; 40-4F
      0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 ;; 50-5F
      0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 ;; 60-6F
      0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 ;; 70-7F
      1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 ;; 80-8F
      2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2 ;; 90-9F
      3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3 ;; A0-AF
      3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3 ;; B0-BF
      8  8  4  4  4  4  4  4  4  4  4  4  4  4  4  4 ;; C0-CF
      4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4 ;; D0-DF
      9  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5 ;; E0-EF
      10 6  6  6  7  8  8  8  8  8  8  8  8  8  8  8) ;; F0-FF

 ;; This second table maps a state to a new state when adding a transition.
 ;;   00-7F
 ;;   |  80-8F
 ;;   |  |  90-9F
 ;;   |  |  |  A0-BF
 ;;   |  |  |  |  C2-DF
 ;;   |  |  |  |  |  E1-EF
 ;;   |  |  |  |  |  |  F1-F3
 ;;   |  |  |  |  |  |  |  F4
 ;;   |  |  |  |  |  |  |  |  C0, C1, F5-FF
 ;;   |  |  |  |  |  |  |  |  |  E0
 ;;   |  |  |  |  |  |  |  |  |  |  F0
 #vu8(0  0  0  0  0  0  0  0  0  0  0   ;; REJECT = 0
      11 0  0  0  22 33 44 55 0  66 77 ;; ACCEPT = 11
      0  11 11 11 0  0  0  0  0  0  0  ;; 2-byte = 22
      0  22 22 22 0  0  0  0  0  0  0  ;; 3-byte = 33
      0  33 33 33 0  0  0  0  0  0  0  ;; 4-byte = 44
      0  33 0  0  0  0  0  0  0  0  0  ;; 4-byte low = 55
      0  0  0  22 0  0  0  0  0  0  0  ;; 3-byte high = 66
      0  0  33 33 0  0  0  0  0  0  0))  ;; 4-byte mid/high = 77

(func $decode-wtf8 (param $byte i32) (param $buf i32) (param $state i32)
      (result i32 i32) ; codepoint, state
      (local $type i32)
      (local.set $type (i32.load8_u $decoder (local.get $byte)))
      ;; Two values: first push the codepoint
      (i32.or (i32.shl (local.get $buf) (i32.const 6))
              (i32.and (local.get $byte)
                       (i32.shr_u (i32.const #x7f)
                                  (i32.shr_u (local.get $type)
                                             (i32.const 1)))))
      ;; Then the state
      (i32.load8_u $decoder offset=256
                   (i32.add (local.get $state) (local.get $type))))

(func $string_iter (export "as_iter")
      (param $wtf8 (ref $wtf8)) (result (ref $iter))
      (struct.new $iter (local.get $wtf8) (i32.const 0)))

(func $iter_next (export "iter_next")
      (param $iter (ref $iter))
      (result i32)
      (local $wtf8 (ref $wtf8))
      (local $cp i32)
      (local $state i32)
      (local $i i32)
      (local.set $wtf8 (struct.get $iter $wtf8 (local.get $iter)))
      (local.set $i (struct.get $iter $pos (local.get $iter)))
      (local.set $state (i32.const 11)) ;; ACCEPT
      (if (i32.ge_u (local.get $i) (array.len (local.get $wtf8)))
          (then (return (i32.const -1))))
      (loop $lp
        (if (i32.ge_u (local.get $i) (array.len (local.get $wtf8)))
            ;; Bad WTF-8.
            (then (unreachable)))
        (call $decode-wtf8
              (array.get_u $wtf8 (local.get $wtf8) (local.get $i))
              (local.get $cp)
              (local.get $state))
        (local.set $state)
        (local.set $cp)
        ;; Must be valid WTF-8!
        (if (i32.eq (local.get $state) (i32.const 0)) ;; REJECT
            (then (unreachable)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (if (i32.ne (local.get $state) (i32.const 11)) ;; ACCEPT
            (then (br $lp))))
      (struct.set $iter $pos (local.get $iter) (local.get $i))
      (local.get $cp))

(func $string_builder (export "string_builder")
      (result (ref $builder))
      (struct.new $builder
                  (array.new_default $wtf8 (i32.const 256))
                  (i32.const 0)))

(func $builder_push_codepoint (export "builder_push_codepoint")
      (param $builder (ref $builder))
      (param $cp i32)
      (local $wtf8 (ref $wtf8))
      (local $pos i32)
      (local.set $wtf8 (struct.get $builder $wtf8 (local.get $builder)))
      (local.set $pos (struct.get $builder $pos (local.get $builder)))
      (if (i32.lt_u (array.len (local.get $wtf8))
                    (i32.add (i32.const 4) (local.get $pos)))
          (then
           (local.set $wtf8
                      (array.new_default
                       $wtf8
                       (i32.shl (array.len (local.get $wtf8)) (i32.const 1))))
           (array.copy $wtf8 $wtf8
                       (local.get $wtf8) (i32.const 0)
                       (struct.get $builder $wtf8 (local.get $builder))
                       (i32.const 0) (local.get $pos))
           (struct.set $builder $wtf8 (local.get $builder)
                       (local.get $wtf8))))
      (if (i32.le_u (local.get $cp) (i32.const #x7f))
          (then
           (array.set $wtf8 (local.get $wtf8) (local.get $pos)
                      (local.get $cp))
           (struct.set $builder $pos (local.get $builder)
                       (i32.add (local.get $pos) (i32.const 1)))
           (return)))
      (if (i32.le_u (local.get $cp) (i32.const #x7ff))
          (then
           (array.set $wtf8 (local.get $wtf8) (local.get $pos)
                      (i32.or (i32.shr_u (local.get $cp) (i32.const 6))
                              (i32.const #b11000000)))
           (array.set $wtf8 (local.get $wtf8)
                      (i32.add (local.get $pos) (i32.const 1))
                      (i32.or (i32.and (local.get $cp) (i32.const #b00111111))
                              (i32.const #b10000000)))
           (struct.set $builder $pos (local.get $builder)
                       (i32.add (local.get $pos) (i32.const 2)))
           (return)))
      (if (i32.le_u (local.get $cp) (i32.const #xffff))
          (then
           (array.set $wtf8 (local.get $wtf8) (local.get $pos)
                      (i32.or (i32.shr_u (local.get $cp) (i32.const 12))
                              (i32.const #b11100000)))
           (array.set $wtf8 (local.get $wtf8)
                      (i32.add (local.get $pos) (i32.const 1))
                      (i32.or (i32.and (i32.shr_u (local.get $cp) (i32.const 6))
                                       (i32.const #b00111111))
                              (i32.const #b10000000)))
           (array.set $wtf8 (local.get $wtf8)
                      (i32.add (local.get $pos) (i32.const 2))
                      (i32.or (i32.and (local.get $cp) (i32.const #b00111111))
                              (i32.const #b10000000)))
           (struct.set $builder $pos (local.get $builder)
                       (i32.add (local.get $pos) (i32.const 3)))
           (return)))
      (array.set $wtf8 (local.get $wtf8) (local.get $pos)
                 (i32.or (i32.shr_u (local.get $cp) (i32.const 18))
                         (i32.const #b11110000)))
      (array.set $wtf8 (local.get $wtf8)
                 (i32.add (local.get $pos) (i32.const 1))
                 (i32.or (i32.and (i32.shr_u (local.get $cp) (i32.const 12))
                                  (i32.const #b00111111))
                         (i32.const #b10000000)))
      (array.set $wtf8 (local.get $wtf8)
                 (i32.add (local.get $pos) (i32.const 2))
                 (i32.or (i32.and (i32.shr_u (local.get $cp) (i32.const 6))
                                  (i32.const #b00111111))
                         (i32.const #b10000000)))
      (array.set $wtf8 (local.get $wtf8)
                 (i32.add (local.get $pos) (i32.const 3))
                 (i32.or (i32.and (local.get $cp) (i32.const #b00111111))
                         (i32.const #b10000000)))
      (struct.set $builder $pos (local.get $builder)
                  (i32.add (local.get $pos) (i32.const 4))))

(func $builder_finish (export "finish_builder")
      (param $builder (ref $builder)) (result (ref $wtf8))
      (local $wtf8 (ref $wtf8))
      (local $len i32)
      (local.set $len (struct.get $builder $pos (local.get $builder)))
      (local.set $wtf8 (array.new_default $wtf8 (local.get $len)))
      (array.copy $wtf8 $wtf8
                  (local.get $wtf8) (i32.const 0)
                  (struct.get $builder $wtf8 (local.get $builder))
                  (i32.const 0) (local.get $len))
      (local.get $wtf8))
