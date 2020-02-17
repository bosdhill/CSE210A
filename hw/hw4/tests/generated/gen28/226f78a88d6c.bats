load ../../harness

@test "226f78a88d6c" {
  check 'x := 1   -  -3     ' '⇒ skip, {x → 4}'
}
