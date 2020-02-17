load ../../harness

@test "c40730266dcc" {
  check 'if (true∧ false)      then  



skip     else y := 0    ' '⇒ y := 0, {}
⇒ skip, {y → 0}'
}
