load ../../harness

@test "daf63c419df9" {
  check 'skip    ;y    :=  2  + -2   ' '⇒ y := (2+-2), {}
⇒ skip, {y → 0}'
}
