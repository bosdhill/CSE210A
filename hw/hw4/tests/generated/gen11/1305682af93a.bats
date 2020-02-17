load ../../harness

@test "1305682af93a" {
  check 'while 1   *  4     =  -4     -  -4 ∨   SC     -   x<    z   -  v do O   :=  -3  -   z   ' '⇒ skip, {}'
}
