load ../../harness

@test "d7b014bf1db9" {
  check 'if (-4    +    2    =x     -  z     ∨    true)      then   
skip   else 
skip     ' '⇒ skip, {}'
}
