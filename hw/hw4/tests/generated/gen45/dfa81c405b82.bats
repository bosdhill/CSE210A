load ../../harness

@test "dfa81c405b82" {
  check 'if (o     *  i =   y* -4     ∧   z     +z<    -4   -    -4) then 

z   := x  *x    *    y     else 
 
y  := 2     *-2' '⇒ y := (2*-2), {}
⇒ skip, {y → -4}'
}
