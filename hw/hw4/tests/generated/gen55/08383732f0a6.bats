load ../../harness

@test "08383732f0a6" {
  check 'z := y-1 ; skip  ' '⇒ skip; skip, {z → -1}
⇒ skip, {z → -1}'
}
