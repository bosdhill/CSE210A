load ../../harness

@test "b6b2065a9606" {
  check 'while (¬true)  do    skip    ' '⇒ skip, {}'
}
