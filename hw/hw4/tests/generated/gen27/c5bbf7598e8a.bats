load ../../harness

@test "c5bbf7598e8a" {
  check 'z    :=   2  +    -3   ;z    :=    -2+  1  ' '⇒ skip; z := (-2+1), {z → -1}
⇒ z := (-2+1), {z → -1}
⇒ skip, {z → -1}'
}
