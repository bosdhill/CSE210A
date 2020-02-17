load ../../harness

@test "79fc5f09e824" {
  check 'if (y+x < 1 *  -4∧ x  +  1  <   -1   *z)     then 
  x:= 2+ -2      else    skip  ' '⇒ skip, {}'
}
