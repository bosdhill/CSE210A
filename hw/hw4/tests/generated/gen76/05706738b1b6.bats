load ../../harness

@test "05706738b1b6" {
  check 'x  :=  3- 2; 
kw  :=   1     *   z   ' '⇒ skip; kw := (1*z), {x → 1}
⇒ kw := (1*z), {x → 1}
⇒ skip, {kw → 0, x → 1}'
}
