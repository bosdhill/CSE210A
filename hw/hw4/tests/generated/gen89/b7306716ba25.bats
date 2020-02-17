load ../../harness

@test "b7306716ba25" {
  check 'while false∨   -2  -   4   =    z   * 0 do  z    :=  -1 *   z   ' '⇒ skip, {}'
}
