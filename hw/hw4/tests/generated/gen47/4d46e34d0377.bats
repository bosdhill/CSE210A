load ../../harness

@test "4d46e34d0377" {
  check 'if (false     ∧     true)   then 
 skip     else   
skip   ' '⇒ skip, {}'
}
