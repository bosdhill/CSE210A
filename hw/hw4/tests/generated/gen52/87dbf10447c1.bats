load ../../harness

@test "87dbf10447c1" {
  check 'if (¬true)   then  
skip      else 
L:= 0  *  z     ' '⇒ L := (0*z), {}
⇒ skip, {L → 0}'
}
