load ../../harness

@test "283e1710e004" {
  check 'while (¬true)   do skip   ' '⇒ skip, {}'
}
