load ../../harness

@test "ed82d0796cdc" {
  check 'x:= x     -4;skip    ' '⇒ skip; skip, {x → -4}
⇒ skip, {x → -4}'
}
