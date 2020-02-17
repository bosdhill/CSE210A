load ../../harness

@test "5cb5ccf599c2" {
  check 'x    :=    -2;
    skip' '⇒ skip; skip, {x → -2}
⇒ skip, {x → -2}'
}
