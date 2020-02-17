load ../../harness

@test "6130cf4e5145" {
  check 'if (true  ∨  x    + x  =     x     -     z)    then    

z    :=-1   +    y else 
skip ' '⇒ z := (-1+y), {}
⇒ skip, {z → -1}'
}
