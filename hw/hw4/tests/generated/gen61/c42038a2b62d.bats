load ../../harness

@test "c42038a2b62d" {
  check 'if (x   +     3 =   3   + 0∧true) then z     := 2 +2   else h:=   2    + K ' '⇒ z := (2+2), {}
⇒ skip, {z → 4}'
}
