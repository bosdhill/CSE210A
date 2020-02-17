load ../../harness

@test "f9112b631068" {
  check 'x     := -2   -  y    ' '⇒ skip, {x → -2}'
}
