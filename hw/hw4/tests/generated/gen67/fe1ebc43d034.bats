load ../../harness

@test "fe1ebc43d034" {
  check 'x :=     -1   *  y ; 
skip    ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
