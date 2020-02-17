load ../../harness

@test "c41b3731df31" {
  check 'skip;y  :=    3     *  4 ' '⇒ y := (3*4), {}
⇒ skip, {y → 12}'
}
