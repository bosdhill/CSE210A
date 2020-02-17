load ../../harness

@test "f6d746288df1" {
  check 'if (true∧  -1     -1  <     3   -0) then   
x := -1 else    skip   ' '⇒ x := -1, {}
⇒ skip, {x → -1}'
}
