load ../../harness

@test "86ba3a2664f2" {
  check 'x:=K0+ x ;  z  :=   rw     *    y     ' '⇒ skip; z := (rw*y), {x → 0}
⇒ z := (rw*y), {x → 0}
⇒ skip, {x → 0, z → 0}'
}
