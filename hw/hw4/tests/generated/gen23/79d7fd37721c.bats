load ../../harness

@test "79d7fd37721c" {
  check 'while (¬true) do y :=     2 *    z' '⇒ skip, {}'
}
