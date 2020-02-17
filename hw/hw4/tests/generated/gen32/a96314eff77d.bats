load ../../harness

@test "a96314eff77d" {
  check 'if false   then  
 y    := 2   +     z    else  
X:=   -4     * -3   ' '⇒ X := (-4*-3), {}
⇒ skip, {X → 12}'
}
