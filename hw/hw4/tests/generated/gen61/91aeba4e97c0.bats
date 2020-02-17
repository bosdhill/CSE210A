load ../../harness

@test "91aeba4e97c0" {
  check 'if (true ∨true)      then 
 skip    else 

 skip    ' '⇒ skip, {}'
}
