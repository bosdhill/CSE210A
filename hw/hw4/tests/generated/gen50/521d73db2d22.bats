load ../../harness

@test "521d73db2d22" {
  check 'x    :=  y - 1; z     :=   -4    * z    ' '⇒ skip; z := (-4*z), {x → -1}
⇒ z := (-4*z), {x → -1}
⇒ skip, {x → -1, z → 0}'
}
