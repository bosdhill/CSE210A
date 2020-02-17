load ../../harness

@test "4ffe100d31f5" {
  check 'while (¬true)    do skip     ' '⇒ skip, {}'
}
