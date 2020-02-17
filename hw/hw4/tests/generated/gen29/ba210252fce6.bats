load ../../harness

@test "ba210252fce6" {
  check 'if (false   ∧     true)   then skip     else  x     :=     -3     ' '⇒ x := -3, {}
⇒ skip, {x → -3}'
}
