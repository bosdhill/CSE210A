load ../../harness

@test "ea2759481b42" {
  check 'if (¬false)    then  nq   :=  x   +  B   else 
  y    :=y   +z  ' '⇒ nq := (x+B), {}
⇒ skip, {nq → 0}'
}
