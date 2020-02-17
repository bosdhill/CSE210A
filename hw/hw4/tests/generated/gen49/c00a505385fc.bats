load ../../harness

@test "c00a505385fc" {
  check 'if (¬false)    then   
 skip  else skip   ' '⇒ skip, {}'
}
