load ../../harness

@test "83913630fc9e" {
  check 'x :=D3   -    -4; 
  i :=     -2   *   x    ' '⇒ skip; i := (-2*x), {x → 4}
⇒ i := (-2*x), {x → 4}
⇒ skip, {i → -8, x → 4}'
}
