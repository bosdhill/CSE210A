load ../../harness

@test "b747e40422a2" {
  check 'if (WY     *     -3 = z*0 ∧    -1  -   -4 =  z+     x)     then  skip    else 

x:= -3 + 2     ' '⇒ x := (-3+2), {}
⇒ skip, {x → -1}'
}
