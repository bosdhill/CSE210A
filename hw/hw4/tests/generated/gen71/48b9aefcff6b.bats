load ../../harness

@test "48b9aefcff6b" {
  check 'while (¬true)     do z  :=   4    -    x    ' '⇒ skip, {}'
}
