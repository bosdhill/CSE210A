load ../../harness

@test "d958cb093cf5" {
  check 'if (¬false) then   Dv:=z     -     3  else    
skip    ' '⇒ Dv := (z-3), {}
⇒ skip, {Dv → -3}'
}
