load ../../harness

@test "55dcabd3e27d" {
  check 'x    :=  z*     1   ;  W4 :=   3   *z     ' '⇒ skip; W4 := (3*z), {x → 0}
⇒ W4 := (3*z), {x → 0}
⇒ skip, {W4 → 0, x → 0}'
}
