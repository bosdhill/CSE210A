load ../../harness

@test "a1062b4f1d04" {
  check 'if (true  ∨   false) then  x    :=y    +1     else    
 k  :=   4   +   -3' '⇒ x := (y+1), {}
⇒ skip, {x → 1}'
}
