load ../../harness

@test "b16426c0c0a6" {
  check 'skip  ;i   :=-2   +1    ' '⇒ i := (-2+1), {}
⇒ skip, {i → -1}'
}
