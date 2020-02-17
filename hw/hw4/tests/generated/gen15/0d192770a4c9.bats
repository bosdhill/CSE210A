load ../../harness

@test "0d192770a4c9" {
  check 'skip     ;y     :=  1   -   -3  ' '⇒ y := (1--3), {}
⇒ skip, {y → 4}'
}
