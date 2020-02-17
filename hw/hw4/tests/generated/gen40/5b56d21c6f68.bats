load ../../harness

@test "5b56d21c6f68" {
  check 'y    :=   -1  * x' '⇒ skip, {y → 0}'
}
