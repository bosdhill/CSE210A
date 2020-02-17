load ../../harness

@test "3fc4da950d90" {
  check 'z:=3   +   x;z    := 1    ' '⇒ skip; z := 1, {z → 3}
⇒ z := 1, {z → 3}
⇒ skip, {z → 1}'
}
