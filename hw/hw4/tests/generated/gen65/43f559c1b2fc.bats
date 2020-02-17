load ../../harness

@test "43f559c1b2fc" {
  check 'while true ∧ false      do 
skip    ' '⇒ skip, {}'
}
