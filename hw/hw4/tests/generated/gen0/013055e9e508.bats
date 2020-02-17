load ../../harness

@test "013055e9e508" {
  check 'if (true   ∧  1  - E0 <S)    then  

y  :=     x-   -3     else z     :=     -4   +  3     ' '⇒ z := (-4+3), {}
⇒ skip, {z → -1}'
}
