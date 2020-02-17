load ../../harness

@test "7f1837115b00" {
  check 'while 1     - -1     <   1     *  y    ∧  true    do skip     ' '⇒ skip, {}'
}
