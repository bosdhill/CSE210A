load ../../harness

@test "6606a9b4b93b" {
  check 'while (¬true)      do y    := -4    -    1    ' '⇒ skip, {}'
}
