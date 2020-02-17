load ../../harness

@test "6f4678931555" {
  check 'while (¬true)      do skip   ' '⇒ skip, {}'
}
