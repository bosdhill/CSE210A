load ../../harness

@test "571dfa01c257" {
  check 'if (D9- x     =   L + x  ∧    true)     then y :=   1 +   z else 
  skip' '⇒ y := (1+z), {}
⇒ skip, {y → 1}'
}
