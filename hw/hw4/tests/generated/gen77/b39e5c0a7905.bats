load ../../harness

@test "b39e5c0a7905" {
  check 'if (false    ∧     false) then   
skip  else x     := -1    + -1   ' '⇒ x := (-1+-1), {}
⇒ skip, {x → -2}'
}
