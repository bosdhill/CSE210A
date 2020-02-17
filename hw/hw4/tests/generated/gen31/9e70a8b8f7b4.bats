load ../../harness

@test "9e70a8b8f7b4" {
  check 'x    :=  4 *     -4 ;  y    :=   -1     --2   ' '⇒ skip; y := (-1--2), {x → -16}
⇒ y := (-1--2), {x → -16}
⇒ skip, {x → -16, y → 1}'
}
