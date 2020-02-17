load ../../harness

@test "4a07d549b94c" {
  check 'y     :=   1     +     -2  ;x     :=   2   +     -2 ' '⇒ skip; x := (2+-2), {y → -1}
⇒ x := (2+-2), {y → -1}
⇒ skip, {x → 0, y → -1}'
}
