load ../../harness

@test "a5b8e83c40e4" {
  check 'y :=  -2*   2     ;x  :=     y     ' '⇒ skip; x := y, {y → -4}
⇒ x := y, {y → -4}
⇒ skip, {x → -4, y → -4}'
}
