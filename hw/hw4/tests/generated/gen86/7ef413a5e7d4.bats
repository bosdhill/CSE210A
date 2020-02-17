load ../../harness

@test "7ef413a5e7d4" {
  check 'if (true   ∨z + x     <  2    +    -4)   then 

 
z:=   x -y    else 
 
z :=   2    + -4 ' '⇒ z := (x-y), {}
⇒ skip, {z → 0}'
}
