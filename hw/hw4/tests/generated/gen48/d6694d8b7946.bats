load ../../harness

@test "d6694d8b7946" {
  check 'if (iA   -  t     =  C -    z ∨   false)    then x :=     1    +-3   else y:=    4    ' '⇒ x := (1+-3), {}
⇒ skip, {x → -2}'
}
