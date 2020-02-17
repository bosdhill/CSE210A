load ../../harness

@test "6d470e92eeb8" {
  check 'while (¬true)     do skip     ' '⇒ skip, {}'
}
