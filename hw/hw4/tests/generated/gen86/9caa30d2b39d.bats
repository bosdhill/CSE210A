load ../../harness

@test "9caa30d2b39d" {
  check 'x  :=-3     ; x:=     0    *   y  ' '⇒ skip; x := (0*y), {x → -3}
⇒ x := (0*y), {x → -3}
⇒ skip, {x → 0}'
}
