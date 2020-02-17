load ../../harness

@test "eea22ab5efec" {
  check 'x :=x    * z ; 
skip     ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
