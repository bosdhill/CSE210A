load ../../harness

@test "eb7a1873c6a6" {
  check 'if (false ∧  0   +x    =   3    +    y)   then      i :=   z+ -4  else 
  y    :=  j-  2' '⇒ y := (j-2), {}
⇒ skip, {y → -2}'
}
