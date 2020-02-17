load ../../harness

@test "868676789567" {
  check 'x  :=     1;

skip     ' '⇒ skip; skip, {x → 1}
⇒ skip, {x → 1}'
}
