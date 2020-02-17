load ../../harness

@test "d80577fc6aa7" {
  check 'x     :=    x+     z; 
skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
