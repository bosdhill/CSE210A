load ../../harness

@test "4132b91a7c14" {
  check 'skip    ;z     :=4 ' '⇒ z := 4, {}
⇒ skip, {z → 4}'
}
