load ../../harness

@test "6787599b69ea" {
  check 'if (¬true)    then 

skip   else 
 
 skip     ' '⇒ skip, {}'
}
