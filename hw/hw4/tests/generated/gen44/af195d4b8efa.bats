load ../../harness

@test "af195d4b8efa" {
  check 'z    :=   1   +  -2 ;skip' '⇒ skip; skip, {z → -1}
⇒ skip, {z → -1}'
}
