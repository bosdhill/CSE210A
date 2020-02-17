load ../../harness

@test "33b60ba2fb04" {
  check 'while z    = x    +  2    ∧ -4 +  y     <  -2 *y   do  
x :=    y -  -3' '⇒ skip, {}'
}
