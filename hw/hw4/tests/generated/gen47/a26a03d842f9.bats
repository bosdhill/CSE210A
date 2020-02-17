load ../../harness

@test "a26a03d842f9" {
  check 'if (2     *   4 <0     +   z    ∨     -1   *     2     <-1)   then  skip   else  skip    ' '⇒ skip, {}'
}
