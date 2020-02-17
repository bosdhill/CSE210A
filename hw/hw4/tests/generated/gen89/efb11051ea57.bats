load ../../harness

@test "efb11051ea57" {
  check 'x :=     4  -    0    ; 
  y   :=    4   +     1   ' '⇒ skip; y := (4+1), {x → 4}
⇒ y := (4+1), {x → 4}
⇒ skip, {x → 4, y → 5}'
}
