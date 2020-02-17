load ../../harness

@test "e6d53e239c22" {
  check 'while (¬true)  do Ln     :=   2  *   x    ' '⇒ skip, {}'
}
