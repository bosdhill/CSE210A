load ../../harness

@test "69a39f068885" {
  check 'if (4     -  0   <    z   +  x   ∨   x     +d   <     3    -  z)      then  
 z   := 1 *     -4 else 


z   :=   4    +    -2     ' '⇒ z := (1*-4), {}
⇒ skip, {z → -4}'
}
