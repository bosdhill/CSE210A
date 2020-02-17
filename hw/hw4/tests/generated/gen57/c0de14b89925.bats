load ../../harness

@test "c0de14b89925" {
  check 'while (¬true)  do skip     ' '⇒ skip, {}'
}
