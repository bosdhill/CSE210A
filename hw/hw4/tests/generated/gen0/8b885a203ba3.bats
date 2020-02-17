load ../../harness

@test "8b885a203ba3" {
  check 'y :=    4*x   ;skip  ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
