load ../../harness

@test "bfe5b4103724" {
  check 'while y     <    -1 *x  ∧    z  < -4     *z    do    
 y   :=    -4 *     x *  N ' '⇒ skip, {}'
}
