load ../../harness

@test "bc57b879f42c" {
  check 'if (¬(-3   - z    =   y   *     4))   then  
 
y    :=     z -     1  else 
z   :=     -3   + 4    ' '⇒ y := (z-1), {}
⇒ skip, {y → -1}'
}
