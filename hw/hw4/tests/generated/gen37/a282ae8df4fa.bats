load ../../harness

@test "a282ae8df4fa" {
  check 'if (true ∧   false) then 

skip   else  

  skip    ' '⇒ skip, {}'
}
