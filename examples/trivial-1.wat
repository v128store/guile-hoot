(module
  (type $kvarargs (func (param i32) (result i32)))

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

  ;; When the module is instantiated, grow a new default-sized $argv and
  ;; $return-stack.
  (func $start
    (call $grow-argv (i32.const 42))
    (call $grow-return-stack (i32.const 42)))
  (start $start)

  (func $set_arg (export "set_arg") (param $idx i32) (param $arg (ref eq))
    (table.set $argv (local.get $idx) (local.get $arg)))
  (func $get_arg (export "get_arg") (param $idx i32) (result (ref eq))
    (ref.as_non_null (table.get $argv (local.get $idx))))

  (func $return (param $nargs i32) (result i32)
    (local.get $nargs))

  (func $push-return (param $k (ref $kvarargs))
    (table.set $return-stack (global.get $return-sp) (local.get $k))
    (global.set $return-sp (i32.add (global.get $return-sp) (i32.const 1))))
  (func $pop-return (result (ref $kvarargs))
    (global.set $return-sp (i32.sub (global.get $return-sp) (i32.const 1)))
    (ref.as_non_null (table.get $return-stack (global.get $return-sp))))

  ;; Return a fixnum-tagged 1: (1 << 1) | 1.
  (func $main (param $nargs i32) (result i32)
    (table.set $argv (i32.const 0) (i31.new (i32.const 3)))
    (return_call_ref $kvarargs (i32.const 1) (call $pop-return)))

  (func $init (export "_init") (param $nargs i32) (result i32)
    (call $push-return (ref.func $return))
    (return_call $main (local.get $nargs))))
