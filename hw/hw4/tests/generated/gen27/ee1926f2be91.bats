load ../../harness

@test "ee1926f2be91" {
  check 'if (true     ∧    y     +   BD  <    4  -  z)  then 
 y:=     tG +  -3      else 
y  :=z   + z   ' '⇒ y := (tG+-3), {}
⇒ skip, {y → -3}'
}
