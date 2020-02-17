load ../../harness

@test "9292aa95ace2" {
  check 'if (¬false)  then  
  skip else x  :=y     *-2    ' '⇒ skip, {}'
}
