load ../../harness

@test "c87c4743f194" {
  check 'z     :=     1   +    z     ' '⇒ skip, {z → 1}'
}
