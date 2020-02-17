load ../../harness

@test "d64c94eb6702" {
  check 'z     :=    2  ;y    := x     *    y  ' '⇒ skip; y := (x*y), {z → 2}
⇒ y := (x*y), {z → 2}
⇒ skip, {y → 0, z → 2}'
}
