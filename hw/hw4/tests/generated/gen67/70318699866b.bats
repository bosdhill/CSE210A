load ../../harness

@test "70318699866b" {
  check 'N   :=     x  +-3    ;    skip' '⇒ skip; skip, {N → -3}
⇒ skip, {N → -3}'
}
