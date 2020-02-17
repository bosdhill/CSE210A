load ../../harness

@test "d6d0849f85f0" {
  check 'if (true   ∧  false)  then    
skip  else   

 y :=2    - y    ' '⇒ y := (2-y), {}
⇒ skip, {y → 2}'
}
