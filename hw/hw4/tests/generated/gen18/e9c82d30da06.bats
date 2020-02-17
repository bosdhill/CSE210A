load ../../harness

@test "e9c82d30da06" {
  check 'if false then  
  skip     else  
 x :=z    * -3   ' '⇒ x := (z*-3), {}
⇒ skip, {x → 0}'
}
