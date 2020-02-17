load ../../harness

@test "eb4942fce708" {
  check 'y     :=    x+ 1     ; skip  ' '⇒ skip; skip, {y → 1}
⇒ skip, {y → 1}'
}
