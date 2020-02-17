load ../../harness

@test "c484e1ee901e" {
  check 'if (true∧     z    +  -4    <   z- y) then 
   M   :=   -1     * x    else 
y   := z +3  ' '⇒ M := (-1*x), {}
⇒ skip, {M → 0}'
}
