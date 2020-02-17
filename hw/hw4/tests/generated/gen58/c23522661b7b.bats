load ../../harness

@test "c23522661b7b" {
  check 'if (false     ∧   false)      then    

skip   else x  :=     y    + x   ' '⇒ x := (y+x), {}
⇒ skip, {x → 0}'
}
