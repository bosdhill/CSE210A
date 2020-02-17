load ../../harness

@test "5426719c01d7" {
  check 'if (z    *   -1   <4 + x    ∨ false) then 
 
 
skip      else skip   ' '⇒ skip, {}'
}
