load ../../harness

@test "96c5629f9922" {
  check 'if (¬(z  +  3   <   y     *-3))  then    y    :=     y+     z else 
y     :=  -3  +    y ' '⇒ y := (y+z), {}
⇒ skip, {y → 0}'
}
