load ../../harness

@test "86186b8606fc" {
  check 'if (¬true)  then   
skip    else 
skip   ' '⇒ skip, {}'
}
