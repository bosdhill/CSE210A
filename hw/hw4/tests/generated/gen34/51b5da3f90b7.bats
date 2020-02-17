load ../../harness

@test "51b5da3f90b7" {
  check 'if (false     ∨false)      then    
y  := 3* y else a  :=    0   +y     ' '⇒ a := (0+y), {}
⇒ skip, {a → 0}'
}
