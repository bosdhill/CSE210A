load ../../harness

@test "efa298e1d342" {
  check 'if (¬(-2    +  3 <    -4    -    0)) then    z :=     -1    -    -1     else  skip   ' '⇒ z := (-1--1), {}
⇒ skip, {z → 0}'
}
