load ../../harness

@test "42c3b5ee7f29" {
  check 'z     :=   2     *  A    ;z := 2    ' '⇒ skip; z := 2, {z → 0}
⇒ z := 2, {z → 0}
⇒ skip, {z → 2}'
}
