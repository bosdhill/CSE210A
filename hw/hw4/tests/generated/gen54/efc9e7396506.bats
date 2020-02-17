load ../../harness

@test "efc9e7396506" {
  check 'if (¬true)    then  
 skip  else skip     ' '⇒ skip, {}'
}
