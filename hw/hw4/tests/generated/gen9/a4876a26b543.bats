load ../../harness

@test "a4876a26b543" {
  check 'if (y    -0  =4-z)  then 
 skip      else  
 skip' '⇒ skip, {}'
}
