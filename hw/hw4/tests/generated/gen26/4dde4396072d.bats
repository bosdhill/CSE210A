load ../../harness

@test "4dde4396072d" {
  check 'if (¬false) then  Bz :=  P* 0     else  x:= y    *  -3  ' '⇒ Bz := (P*0), {}
⇒ skip, {Bz → 0}'
}
