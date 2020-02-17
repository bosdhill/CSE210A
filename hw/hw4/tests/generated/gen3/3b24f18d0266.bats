load ../../harness

@test "3b24f18d0266" {
  check 'if (true  ∧  false)      then 
skip   else   
skip     ' '⇒ skip, {}'
}
