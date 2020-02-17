load ../../harness

@test "1cd15d26fe06" {
  check 'if (¬(-1     - -2 <0   +  t5)) then   
  y :=     2    -     -3   else  
skip' '⇒ y := (2--3), {}
⇒ skip, {y → 5}'
}
