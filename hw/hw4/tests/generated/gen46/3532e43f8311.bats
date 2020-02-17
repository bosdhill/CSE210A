load ../../harness

@test "3532e43f8311" {
  check 'while -2    + -2 = y  *  4   ∧  false  do x   :=    y  + 0' '⇒ skip, {}'
}
