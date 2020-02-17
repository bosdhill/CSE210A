load ../../harness

@test "430407e128de" {
  check 'if (¬false)      then  

skip  else  

skip  ' '⇒ skip, {}'
}
