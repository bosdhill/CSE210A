load ../../harness

@test "23042b6ada12" {
  check 'if (¬true)    then 

 
skip   else  
  skip   ' '⇒ skip, {}'
}
