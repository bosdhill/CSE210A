load ../../harness

@test "b65b5425a097" {
  check 'if (1 +  z     =    -4 ∧    true)  then  
 
skip    else 

   z     :=  2 +  4 ' '⇒ z := (2+4), {}
⇒ skip, {z → 6}'
}
