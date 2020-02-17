load ../../harness

@test "7a4f5071aae5" {
  check 'z:= A   +1;skip    ' '⇒ skip; skip, {z → 1}
⇒ skip, {z → 1}'
}
