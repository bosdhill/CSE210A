load ../../harness

@test "31555ca8b51a" {
  check 'if (-1    *2<y   *    1 ∨   z     *  -4   <-4 +  x)     then 

z    := x   +    -2      else  

skip  ' '⇒ z := (x+-2), {}
⇒ skip, {z → -2}'
}
