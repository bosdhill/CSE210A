load ../../harness

@test "92792f59b542" {
  check 'x     :=   x +     -4;   skip  ' '⇒ skip; skip, {x → -4}
⇒ skip, {x → -4}'
}
