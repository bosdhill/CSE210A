load ../../harness

@test "aa4999041a1f" {
  check 'x   :=     -4   *    x  ; 
skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
