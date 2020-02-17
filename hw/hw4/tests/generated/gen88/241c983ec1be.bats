load ../../harness

@test "241c983ec1be" {
  check 'if (¬(3  *  -3 <1 *0))      then 
skip  else      y :=  0     *Td   ' '⇒ y := (0*Td), {}
⇒ skip, {y → 0}'
}
