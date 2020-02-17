load ../../harness

@test "3ced7d28312f" {
  check 'while (¬true)     do  uu    :=-1     ' '⇒ skip, {}'
}
