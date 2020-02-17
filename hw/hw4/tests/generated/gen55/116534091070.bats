load ../../harness

@test "116534091070" {
  check 'if (false   ∧    true)  then skip else  x:=    y    *     Ap   ' '⇒ x := (y*Ap), {}
⇒ skip, {x → 0}'
}
