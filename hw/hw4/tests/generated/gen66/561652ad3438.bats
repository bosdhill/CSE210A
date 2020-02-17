load ../../harness

@test "561652ad3438" {
  check 'x     :=    z* -2    ;  x     :=   -4*  y     ' '⇒ skip; x := (-4*y), {x → 0}
⇒ x := (-4*y), {x → 0}
⇒ skip, {x → 0}'
}
