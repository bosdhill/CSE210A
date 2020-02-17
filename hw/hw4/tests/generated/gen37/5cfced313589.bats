load ../../harness

@test "5cfced313589" {
  check 'y    :=   -4     +s8  ;     y:=  -4    -   -3' '⇒ skip; y := (-4--3), {y → -4}
⇒ y := (-4--3), {y → -4}
⇒ skip, {y → -1}'
}
