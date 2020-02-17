load ../../harness

@test "bb05726dea34" {
  check 'if (x    =   z   +    y     ∧     3  -x     <    0   *   -1)    then 
skip else  y  :=    0  -     G2 ' '⇒ y := (0-G2), {}
⇒ skip, {y → 0}'
}
