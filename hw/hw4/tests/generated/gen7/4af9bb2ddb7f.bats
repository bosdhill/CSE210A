load ../../harness

@test "4af9bb2ddb7f" {
  check 'skip    ; x :=z     +   -4   ' '⇒ x := (z+-4), {}
⇒ skip, {x → -4}'
}
