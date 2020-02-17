load ../../harness

@test "1e4526cd4a6f" {
  check 'if (3     = -1    *    z ∨ true)    then 
y     :=y     *    -3   else 

z:=   x +     -3' '⇒ y := (y*-3), {}
⇒ skip, {y → 0}'
}
