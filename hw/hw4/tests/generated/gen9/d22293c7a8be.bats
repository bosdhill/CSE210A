load ../../harness

@test "d22293c7a8be" {
  check 'skip;y     :=    y  + s    ' '⇒ y := (y+s), {}
⇒ skip, {y → 0}'
}
