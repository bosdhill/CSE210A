load ../../harness

@test "6d7bf10340dc" {
  check 'skip   ;x:= 4     +     lb' '⇒ x := (4+lb), {}
⇒ skip, {x → 4}'
}
