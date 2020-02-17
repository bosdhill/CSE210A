load ../../harness

@test "349e5a5daf7a" {
  check 'if (true    ∧   false)  then 
y:=     x   -   -1    else    
y    := x     ' '⇒ y := x, {}
⇒ skip, {y → 0}'
}
