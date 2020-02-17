load ../../harness

@test "8166fc296fe6" {
  check 'if (nc     -     -3=     x +    -4   ∧  true)    then 



 y:=   Sg -y  else   y  := Aw     -y' '⇒ y := (Aw-y), {}
⇒ skip, {y → 0}'
}
