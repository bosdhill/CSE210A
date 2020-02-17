load ../../harness

@test "b9e42f621454" {
  check 'if (true   ∨    -1   +   r2  = y    + y) then   
 x   :=  4     *     z    else y :=  y    -x  ' '⇒ x := (4*z), {}
⇒ skip, {x → 0}'
}
