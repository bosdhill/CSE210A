load ../../harness

@test "205b3139f7af" {
  check 'if (¬true)     then  

skip  else   
skip   ' '⇒ skip, {}'
}
