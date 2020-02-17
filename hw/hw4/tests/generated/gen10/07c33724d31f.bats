load ../../harness

@test "07c33724d31f" {
  check 'if (z    +-4  <   z    -     -4    ∨ z    +    z     <     -3    -   y) then  IU     := 2    *z     else  
  x:=  3     ' '⇒ IU := (2*z), {}
⇒ skip, {IU → 0}'
}
