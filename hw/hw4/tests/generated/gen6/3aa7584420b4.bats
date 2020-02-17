load ../../harness

@test "3aa7584420b4" {
  check 'if (¬false)    then x     :=    Ib  +   z      else 
   if (¬false) then  
  l     :=    x + z     else   
 skip ' '⇒ x := (Ib+z), {}
⇒ skip, {x → 0}'
}
