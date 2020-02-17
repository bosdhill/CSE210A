load ../../harness

@test "d64909b204c9" {
  check 'if (z  <  L     +   2 ∨  true)   then 


x := 2    + 4 else 

skip' '⇒ x := (2+4), {}
⇒ skip, {x → 6}'
}
