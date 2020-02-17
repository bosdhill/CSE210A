load ../../harness

@test "308d9add4fd9" {
  check 'while -2     *  y     < -2 *   0 ∧ true    do 
 x    :=  z    +   y  ' '⇒ skip, {}'
}
