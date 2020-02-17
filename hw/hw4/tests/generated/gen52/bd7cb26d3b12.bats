load ../../harness

@test "bd7cb26d3b12" {
  check 'y:=   4 + y  ;   x:=    1*     -4 ' '⇒ skip; x := (1*-4), {y → 4}
⇒ x := (1*-4), {y → 4}
⇒ skip, {x → -4, y → 4}'
}
