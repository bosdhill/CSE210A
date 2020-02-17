load ../../harness

@test "ab95b49339a9" {
  check 'skip ;y:=    z    *  -3  ' '⇒ y := (z*-3), {}
⇒ skip, {y → 0}'
}
