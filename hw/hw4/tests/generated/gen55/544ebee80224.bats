load ../../harness

@test "544ebee80224" {
  check 'if (2     + sO =  0   *    -2   ∧  true)  then skip   else  F3  :=    V + -3 ' '⇒ F3 := (V+-3), {}
⇒ skip, {F3 → -3}'
}
