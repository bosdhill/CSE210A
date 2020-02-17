load ../../harness

@test "c479b8e08202" {
  check 'x :=YL     - me   ;    y     :=  -4*  z     ' '⇒ skip; y := (-4*z), {x → 0}
⇒ y := (-4*z), {x → 0}
⇒ skip, {x → 0, y → 0}'
}
