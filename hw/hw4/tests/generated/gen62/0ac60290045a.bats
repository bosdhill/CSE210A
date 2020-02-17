load ../../harness

@test "0ac60290045a" {
  check 'P   :=  0   *  y; 

x   := y     +     -1   ' '⇒ skip; x := (y+-1), {P → 0}
⇒ x := (y+-1), {P → 0}
⇒ skip, {P → 0, x → -1}'
}
