load ../../harness

@test "fe58852e7646" {
  check 'while 1    -  -3 <     0    *     2    ∨  z  +  y  < -3     *    x do x    :=     2   +1  ' '⇒ skip, {}'
}
