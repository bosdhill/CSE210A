load ../../harness

@test "cf6386b90a89" {
  check 'while (¬true)      do   skip  ' '⇒ skip, {}'
}
