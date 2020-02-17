load ../../harness

@test "327c4a00072c" {
  check 'if (¬false)   then   

 x   :=   3   +     -1    else 
skip     ' '⇒ x := (3+-1), {}
⇒ skip, {x → 2}'
}
