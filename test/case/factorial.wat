(module
    (type (func (param f64) (result f64)))
    (func
        (type 0)
        (if
            (result f64)
            (f64.lt (local.get 0) (f64.const 1.))
            (then (f64.const 1.))
            (else
                (local.get 0)
                (f64.mul (f64.sub (local.get 0) (f64.const 1.)) (call 0)))))
    (export "fac" (func 0)))
