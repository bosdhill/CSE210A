load ../../harness

@test "bf05999080a2" {
  check 'if (¬(x   +   4 <y))  then 
y:=-3     +    -4   else    
 x    := Q  --3 ' '⇒ y := (-3+-4), {}
⇒ skip, {y → -7}'
}
