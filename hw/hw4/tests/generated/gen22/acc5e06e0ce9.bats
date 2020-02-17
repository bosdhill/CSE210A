load ../../harness

@test "acc5e06e0ce9" {
  check 'skip   ;  y:=   z +     3 ' '⇒ y := (z+3), {}
⇒ skip, {y → 3}'
}
