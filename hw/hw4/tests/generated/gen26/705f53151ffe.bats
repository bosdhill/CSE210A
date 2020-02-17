load ../../harness

@test "705f53151ffe" {
  check 'z    := z     -  1   ' '⇒ skip, {z → -1}'
}
