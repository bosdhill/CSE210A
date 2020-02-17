load ../../harness

@test "5ed27dac86c6" {
  check 'if (false     ∨   true)      then    skip     else 
x :=  z   ' '⇒ skip, {}'
}
