load ../../harness

@test "7ef05a7e9241" {
  check 'x    :=    x -  x;  skip   ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
