load ../../harness

@test "a32ba4079584" {
  check 'if (true   ∧     false)      then  

 skip      else 
y    :=  4   + -3' '⇒ y := (4+-3), {}
⇒ skip, {y → 1}'
}
