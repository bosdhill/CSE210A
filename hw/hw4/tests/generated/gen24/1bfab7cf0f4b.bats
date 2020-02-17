load ../../harness

@test "1bfab7cf0f4b" {
  check 'if (¬true)  then  
skip  else 
 
  skip' '⇒ skip, {}'
}
