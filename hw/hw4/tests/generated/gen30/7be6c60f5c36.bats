load ../../harness

@test "7be6c60f5c36" {
  check 'if (false∧    false)      then  
 y     :=x - -2 else    
 y  :=     -4   +     0    ' '⇒ y := (-4+0), {}
⇒ skip, {y → -4}'
}
