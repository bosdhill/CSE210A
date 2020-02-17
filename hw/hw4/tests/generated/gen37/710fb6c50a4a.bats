load ../../harness

@test "710fb6c50a4a" {
  check 'x    :=  y + -4;
  skip  ' '⇒ skip; skip, {x → -4}
⇒ skip, {x → -4}'
}
