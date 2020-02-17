load ../../harness

@test "1b75221d2fa4" {
  check 'if (true∧    0    +   2     <   y  +    4) then 
 
z    :=     x    -    -2     else  y :=-4    -   0' '⇒ z := (x--2), {}
⇒ skip, {z → 2}'
}
