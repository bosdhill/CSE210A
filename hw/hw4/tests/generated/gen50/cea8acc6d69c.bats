load ../../harness

@test "cea8acc6d69c" {
  check 'if true      then  
skip   else 
skip     ' '⇒ skip, {}'
}
