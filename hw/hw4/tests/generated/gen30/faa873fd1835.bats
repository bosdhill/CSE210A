load ../../harness

@test "faa873fd1835" {
  check 'while (¬(y    *     2=     4 *   y)) do skip  ' '⇒ skip, {}'
}
