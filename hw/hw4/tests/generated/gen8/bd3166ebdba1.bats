load ../../harness

@test "bd3166ebdba1" {
  check 'while (¬true)     do z   :=   z* y     ' '⇒ skip, {}'
}
