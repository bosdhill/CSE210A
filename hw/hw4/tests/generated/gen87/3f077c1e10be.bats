load ../../harness

@test "3f077c1e10be" {
  check 'if (2     + -2< z    +   2  ∧ LR    +  v    <   2 *3) then 
y     :=    3   -    3      else 
skip   ' '⇒ y := (3-3), {}
⇒ skip, {y → 0}'
}
