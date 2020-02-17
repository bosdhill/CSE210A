load ../../harness

@test "282c14e349ee" {
  check 'while (¬true)    do skip     ;    x :=     2    *-2  ' '⇒ skip; x := (2*-2), {}
⇒ x := (2*-2), {}
⇒ skip, {x → -4}'
}
