load ../../harness

@test "d64007f55b9b" {
  check 'if (false   ∧    z +   x   <  -3  +     4)  then   
y   := y     - 2      else 

z:=  x-   x     ' '⇒ z := (x-x), {}
⇒ skip, {z → 0}'
}
