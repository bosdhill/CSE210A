load ../../harness

@test "bdb2a94616d0" {
  check 'if (z     -  z     =    x   ∧false)      then 
 
 skip  else 

 {skip ; skip}   ' '⇒ skip; skip, {}
⇒ skip, {}'
}
