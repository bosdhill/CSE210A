load ../../harness

@test "4a590003e525" {
  check 'if (-3    -  -1    < y    -     x    ∧     false) then   
 
skip  else  
  x   := z*  x    ' '⇒ x := (z*x), {}
⇒ skip, {x → 0}'
}
