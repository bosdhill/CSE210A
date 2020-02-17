load ../../harness

@test "9d33199708b1" {
  check 'while (¬true)      do   skip    ' '⇒ skip, {}'
}
