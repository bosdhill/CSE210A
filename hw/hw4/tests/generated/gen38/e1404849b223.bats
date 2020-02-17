load ../../harness

@test "e1404849b223" {
  check 'if (¬true)   then    
 skip    else  

 skip    ' '⇒ skip, {}'
}
