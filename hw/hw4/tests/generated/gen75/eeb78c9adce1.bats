load ../../harness

@test "eeb78c9adce1" {
  check 'skip;y :=    x *     0  ' '⇒ y := (x*0), {}
⇒ skip, {y → 0}'
}
