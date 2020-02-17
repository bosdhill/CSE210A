load ../../harness

@test "0ffab832eb76" {
  check 'x :=    z  +z  ;Ig:=    x   +  -4  ' '⇒ skip; Ig := (x+-4), {x → 0}
⇒ Ig := (x+-4), {x → 0}
⇒ skip, {Ig → -4, x → 0}'
}
