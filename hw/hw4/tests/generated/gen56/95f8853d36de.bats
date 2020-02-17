load ../../harness

@test "95f8853d36de" {
  check 'if (4     <     r   *     z     ∨true)     then  
 skip  else 

 z     :=     z*-2   ' '⇒ skip, {}'
}
