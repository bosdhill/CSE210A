load ../../harness

@test "9d743f0c8859" {
  check 'y   :=  y   +  -3    ;x:=  -4     +     y  ' '⇒ skip; x := (-4+y), {y → -3}
⇒ x := (-4+y), {y → -3}
⇒ skip, {x → -7, y → -3}'
}
