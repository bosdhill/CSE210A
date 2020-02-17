load ../../harness

@test "867ee02b2101" {
  check 'while (¬true)     do skip    ' '⇒ skip, {}'
}
