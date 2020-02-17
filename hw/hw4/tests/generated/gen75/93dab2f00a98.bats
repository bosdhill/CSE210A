load ../../harness

@test "93dab2f00a98" {
  check 'y    :=   z    * 2     ; 
skip' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
