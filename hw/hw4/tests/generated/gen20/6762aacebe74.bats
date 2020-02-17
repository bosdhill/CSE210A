load ../../harness

@test "6762aacebe74" {
  check 'while (¬true)    do      z :=    y + -4   ' '⇒ skip, {}'
}
