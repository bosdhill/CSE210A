load ../../harness

@test "2b0c1b551f92" {
  check 'x:=     2   *  x  ;  
skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
