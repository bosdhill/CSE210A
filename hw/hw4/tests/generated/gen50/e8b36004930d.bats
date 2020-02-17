load ../../harness

@test "e8b36004930d" {
  check 'if (false ∨     false)     then skip      else 

 

{skip  ;
y :=    Y     +1}  ' '⇒ skip; y := (Y+1), {}
⇒ y := (Y+1), {}
⇒ skip, {y → 1}'
}
