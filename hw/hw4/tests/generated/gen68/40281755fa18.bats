load ../../harness

@test "40281755fa18" {
  check 'x    :=   -1    -2' '⇒ skip, {x → -3}'
}
