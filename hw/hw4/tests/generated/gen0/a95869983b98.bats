load ../../harness

@test "a95869983b98" {
  check 'if (y   -y     <    4  ∧   true) then   y :=   y    + y else xg :=     m   -     hR' '⇒ y := (y+y), {}
⇒ skip, {y → 0}'
}
