load ../../harness

@test "17656cce2946" {
  check 'while z    *    -1  =   2    +    -2     ∧ false do    
z :=    1 *    -4   ' '⇒ skip, {}'
}
