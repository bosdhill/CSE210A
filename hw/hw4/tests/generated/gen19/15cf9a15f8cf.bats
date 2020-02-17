load ../../harness

@test "15cf9a15f8cf" {
  check 'if (y   -  -4     <   y    -  k6     ∧true)      then  skip   else 
 y :=   z   + z  ' '⇒ y := (z+z), {}
⇒ skip, {y → 0}'
}
