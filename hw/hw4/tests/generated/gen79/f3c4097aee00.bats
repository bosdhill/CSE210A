load ../../harness

@test "f3c4097aee00" {
  check 'if (y   +    4   = x *  2  ∧  true)  then 

 
skip   else x     :=     2 *    4 ' '⇒ x := (2*4), {}
⇒ skip, {x → 8}'
}
