load ../../harness

@test "1569924e064b" {
  check 'if (x     -  -4=    x *    -1    ∨  -1+2    <    4- 0)     then   
y   :=    z    + 4  else 
   skip ' '⇒ y := (z+4), {}
⇒ skip, {y → 4}'
}
