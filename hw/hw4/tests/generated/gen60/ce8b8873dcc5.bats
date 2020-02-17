load ../../harness

@test "ce8b8873dcc5" {
  check 'if (¬false)    then    x   :=   z +  -3   else 
sv    :=     y* 2' '⇒ x := (z+-3), {}
⇒ skip, {x → -3}'
}
