load ../../harness

@test "816e8ace2a39" {
  check 'x  :=  3   -   1; 
skip' '⇒ skip; skip, {x → 2}
⇒ skip, {x → 2}'
}
