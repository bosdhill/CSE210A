load ../../harness

@test "8cdd1b2e2bec" {
  check 'skip; x  := z     -   3   ' '⇒ x := (z-3), {}
⇒ skip, {x → -3}'
}
