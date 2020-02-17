load ../../harness

@test "e2c300db0ae2" {
  check 'while 2  <-4   *   1    ∧  1    *   -1  <    4    *  3   do   x :=    b +    z    ' '⇒ skip, {}'
}
