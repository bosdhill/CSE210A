load ../../harness

@test "db3fd0d261d9" {
  check 'x  :=    a; skip ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
