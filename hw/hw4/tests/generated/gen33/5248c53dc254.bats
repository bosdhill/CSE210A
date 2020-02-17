load ../../harness

@test "5248c53dc254" {
  check 'n     :=     x*   z  ;
skip' '⇒ skip; skip, {n → 0}
⇒ skip, {n → 0}'
}
