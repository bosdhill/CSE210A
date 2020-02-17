load ../../harness

@test "3e25049e7c50" {
  check 'J :=1   ; x  :=  -3*     0  ' '⇒ skip; x := (-3*0), {J → 1}
⇒ x := (-3*0), {J → 1}
⇒ skip, {J → 1, x → 0}'
}
