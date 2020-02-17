load ../../harness

@test "a70d26d0cc17" {
  check 'while (¬true)   do skip    ' '⇒ skip, {}'
}
