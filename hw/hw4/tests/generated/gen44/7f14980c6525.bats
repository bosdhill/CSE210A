load ../../harness

@test "7f14980c6525" {
  check 'if (4     *   La   <  v    -    3 ∨false)   then 
 

skip    else 
 y:= -1   +  -2  ' '⇒ y := (-1+-2), {}
⇒ skip, {y → -3}'
}
