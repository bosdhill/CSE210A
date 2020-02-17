load ../../harness

@test "18981e2a4451" {
  check 'x:=  x   *    jz    ; T  := 4     -  y  ' '⇒ skip; T := (4-y), {x → 0}
⇒ T := (4-y), {x → 0}
⇒ skip, {T → 4, x → 0}'
}
