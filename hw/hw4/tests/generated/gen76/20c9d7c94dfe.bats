load ../../harness

@test "20c9d7c94dfe" {
  check 'x :=    3     *    -1 ;skip    ' '⇒ skip; skip, {x → -3}
⇒ skip, {x → -3}'
}
