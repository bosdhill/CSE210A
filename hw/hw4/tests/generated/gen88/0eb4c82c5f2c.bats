load ../../harness

@test "0eb4c82c5f2c" {
  check 'if (false  ∧   z   +    z   =     z)      then 
 
 z    :=   z     +    -4    else  
   y  :=  1*    -2' '⇒ y := (1*-2), {}
⇒ skip, {y → -2}'
}
