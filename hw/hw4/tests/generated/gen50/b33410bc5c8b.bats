load ../../harness

@test "b33410bc5c8b" {
  check 'skip; y :=  4  + -4    ' '⇒ y := (4+-4), {}
⇒ skip, {y → 0}'
}
