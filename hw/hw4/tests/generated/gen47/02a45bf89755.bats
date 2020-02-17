load ../../harness

@test "02a45bf89755" {
  check 'x  :=   D     +  x     ; 

x    :=   S     ' '⇒ skip; x := S, {x → 0}
⇒ x := S, {x → 0}
⇒ skip, {x → 0}'
}
