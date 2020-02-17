load ../../harness

@test "be527518aedf" {
  check 'x :=  y     +z   ;   

x  :=  0   +  1   ' '⇒ skip; x := (0+1), {x → 0}
⇒ x := (0+1), {x → 0}
⇒ skip, {x → 1}'
}
