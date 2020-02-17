load ../../harness

@test "0046fab666cd" {
  check 'skip;z:=    y     *   z    ' '⇒ z := (y*z), {}
⇒ skip, {z → 0}'
}
