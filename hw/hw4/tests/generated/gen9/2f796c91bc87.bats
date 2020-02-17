load ../../harness

@test "2f796c91bc87" {
  check 'if (¬false)      then  
t  :=  -4     +   -3      else 
 skip     ' '⇒ t := (-4+-3), {}
⇒ skip, {t → -7}'
}
