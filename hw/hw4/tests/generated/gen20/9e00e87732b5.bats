load ../../harness

@test "9e00e87732b5" {
  check 'skip    ;z     :=     0+  -1   ' '⇒ z := (0+-1), {}
⇒ skip, {z → -1}'
}
