load ../../harness

@test "cf2de987f8a6" {
  check 'x    :=  y    +-4  ' '⇒ skip, {x → -4}'
}
