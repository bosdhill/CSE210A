load ../../harness

@test "8b80f6444033" {
  check 'if (false ∨-1     =   -1 +   z)  then z:=   -2    *F  else 
skip ' '⇒ z := (-2*F), {}
⇒ skip, {z → 0}'
}
