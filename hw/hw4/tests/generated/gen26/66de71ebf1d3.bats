load ../../harness

@test "66de71ebf1d3" {
  check 'if (-1  -   x   =z  +x  ∨  R     -    y<    x   *    y)     then 

 skip   else 
x     :=-4-y   ' '⇒ x := (-4-y), {}
⇒ skip, {x → -4}'
}
