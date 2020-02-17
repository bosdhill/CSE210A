load ../../harness

@test "372399ba2bea" {
  check 'if (¬false)    then     skip  else  
skip  ' '⇒ skip, {}'
}
