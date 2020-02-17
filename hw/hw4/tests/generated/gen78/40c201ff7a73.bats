load ../../harness

@test "40c201ff7a73" {
  check 'if (x   +    -1     =     y--1∧Y4*    x =   2    *    y)    then 
 
skip     else  
 if (¬false)    then    y   :=  y    + B else   
skip' '⇒ if ¬false then { y := (y+B) } else { skip }, {}
⇒ y := (y+B), {}
⇒ skip, {y → 0}'
}
