load ../../harness

@test "c8233db16837" {
  check 'while (¬true)   do   skip' '⇒ skip, {}'
}
