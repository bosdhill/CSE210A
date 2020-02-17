load ../../harness

@test "47b298ae9328" {
  check 'if (¬(rV *    x  <  y  +x)) then   
  skip  else 
   skip' '⇒ skip, {}'
}
