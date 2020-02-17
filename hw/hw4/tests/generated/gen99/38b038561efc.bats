load ../../harness

@test "38b038561efc" {
  check 'while (¬true)    do  y :=x     *  y     ' '⇒ skip, {}'
}
