load ../../harness

@test "165f3f7843e9" {
  check 'if (false ∧y +    dR <   0 * z)    then  
  y     :=z -     3  else   z:=    -2' '⇒ z := -2, {}
⇒ skip, {z → -2}'
}
