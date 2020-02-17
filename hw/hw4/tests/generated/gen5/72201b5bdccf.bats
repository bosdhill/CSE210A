load ../../harness

@test "72201b5bdccf" {
  check 'while (¬true)     do z :=    3    - -4  ' '⇒ skip, {}'
}
