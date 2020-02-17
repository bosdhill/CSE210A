load ../../harness

@test "80468bdbd1f8" {
  check 'x     :=   -1* -2   ' '⇒ skip, {x → 2}'
}
