load ../../harness

@test "ade800412e79" {
  check 'if (vu   * 0    =     x+ z    ∨     true)    then  
skip     else 
 skip' '⇒ skip, {}'
}
