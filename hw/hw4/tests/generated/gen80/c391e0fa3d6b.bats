load ../../harness

@test "c391e0fa3d6b" {
  check 'skip;z:=   y *    z    ' '⇒ z := (y*z), {}
⇒ skip, {z → 0}'
}
