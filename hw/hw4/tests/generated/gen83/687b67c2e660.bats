load ../../harness

@test "687b67c2e660" {
  check 'while (¬true)  do   skip' '⇒ skip, {}'
}
