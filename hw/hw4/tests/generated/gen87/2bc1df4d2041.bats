load ../../harness

@test "2bc1df4d2041" {
  check 'if (DW  -    -3   <     2 ∧    P     =    z  *    -4)  then  skip   else 
z :=-3   * -2   ' '⇒ z := (-3*-2), {}
⇒ skip, {z → 6}'
}
