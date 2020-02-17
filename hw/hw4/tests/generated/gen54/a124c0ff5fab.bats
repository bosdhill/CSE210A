load ../../harness

@test "a124c0ff5fab" {
  check 'if (false ∨ 0    +  D  =   y     +x)   then y     :=     -1 - 3  else   
y     :=-3    +4' '⇒ y := (-1-3), {}
⇒ skip, {y → -4}'
}
