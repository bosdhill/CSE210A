load ../../harness

@test "5b7ce8060edb" {
  check 'x    :=    -1  ;  
x   := i     +     -2    ' '⇒ skip; x := (i+-2), {x → -1}
⇒ x := (i+-2), {x → -1}
⇒ skip, {x → -2}'
}
