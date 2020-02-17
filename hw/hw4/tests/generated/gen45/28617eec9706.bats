load ../../harness

@test "28617eec9706" {
  check 'if (¬(true    ∧   z    -   z     = 4     -     1)) then 

 y  :=     x    -  x  else 
  z:=   4     ' '⇒ y := (x-x), {}
⇒ skip, {y → 0}'
}
