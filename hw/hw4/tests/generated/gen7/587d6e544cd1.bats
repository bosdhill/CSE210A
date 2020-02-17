load ../../harness

@test "587d6e544cd1" {
  check 'if (false ∨   y    -   -3    <jF  -y)    then z:=x    else  
x     :=  2   -    z    ' '⇒ x := (2-z), {}
⇒ skip, {x → 2}'
}
