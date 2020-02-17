load ../../harness

@test "f413dc0c55b9" {
  check 'while z  +   1    < 3     -  4     ∧  3     *   z<    y     -   4  do 
y     :=  z *    y    ' '⇒ skip, {}'
}
