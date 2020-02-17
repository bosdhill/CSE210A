load ../../harness

@test "46c9bb511fdc" {
  check 'skip    ;x    :=     z   - -2     ' '⇒ x := (z--2), {}
⇒ skip, {x → 2}'
}
