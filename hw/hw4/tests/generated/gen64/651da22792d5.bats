load ../../harness

@test "651da22792d5" {
  check 'if (true     ∨ z +-3   <     -2   -  3)      then  
 y     :=     -3*     y    else 
 skip     ' '⇒ y := (-3*y), {}
⇒ skip, {y → 0}'
}
