load ../../harness

@test "f9fbbfa3f786" {
  check 'skip ;y   := G     +   -2' '⇒ y := (G+-2), {}
⇒ skip, {y → -2}'
}
