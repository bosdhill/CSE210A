load ../../harness

@test "5a291f8e875c" {
  check 'if (z *     3<4 +   3    ∨ lP   *    3  <  -2    +     O8) then    
z   := -2   +    2      else 
y := -3  *     2    ' '⇒ z := (-2+2), {}
⇒ skip, {z → 0}'
}
