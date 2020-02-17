load ../../harness

@test "171e48050dcb" {
  check 'y     :=   x -   4   ' '⇒ skip, {y → -4}'
}
