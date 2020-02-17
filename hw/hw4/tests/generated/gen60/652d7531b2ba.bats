load ../../harness

@test "652d7531b2ba" {
  check 'while (¬true)    do y     :=   z  *     yl ' '⇒ skip, {}'
}
