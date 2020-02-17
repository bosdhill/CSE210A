load ../../harness

@test "adf6967cd99f" {
  check 'if (true∧false)   then   
skip      else 
y     :=  2  *     1' '⇒ y := (2*1), {}
⇒ skip, {y → 2}'
}
