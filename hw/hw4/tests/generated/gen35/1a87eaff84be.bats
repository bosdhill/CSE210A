load ../../harness

@test "1a87eaff84be" {
  check 'x :=   x   -    -3 ; 

x :=0*   x     ' '⇒ skip; x := (0*x), {x → 3}
⇒ x := (0*x), {x → 3}
⇒ skip, {x → 0}'
}
