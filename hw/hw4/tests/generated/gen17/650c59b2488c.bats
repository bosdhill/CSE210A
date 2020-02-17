load ../../harness

@test "650c59b2488c" {
  check 'z    :=    i   +    uw     ;z  :=    -1-    y ' '⇒ skip; z := (-1-y), {z → 0}
⇒ z := (-1-y), {z → 0}
⇒ skip, {z → -1}'
}
