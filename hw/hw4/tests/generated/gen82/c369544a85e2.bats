load ../../harness

@test "c369544a85e2" {
  check 'x   :=-4+ x     ;  

x := 0     -   -1     ' '⇒ skip; x := (0--1), {x → -4}
⇒ x := (0--1), {x → -4}
⇒ skip, {x → 1}'
}
