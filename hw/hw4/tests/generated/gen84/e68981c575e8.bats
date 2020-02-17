load ../../harness

@test "e68981c575e8" {
  check 'y     :=x - x; 
skip     ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
