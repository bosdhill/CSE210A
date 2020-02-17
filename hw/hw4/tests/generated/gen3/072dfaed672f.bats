load ../../harness

@test "072dfaed672f" {
  check 'while false ∧x    - -3    = -1   -    1  do x:=z   *     -3    ' '⇒ skip, {}'
}
