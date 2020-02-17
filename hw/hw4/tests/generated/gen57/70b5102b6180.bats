load ../../harness

@test "70b5102b6180" {
  check 'x  :=    y  * 1  ' '⇒ skip, {x → 0}'
}
