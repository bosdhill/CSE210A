load ../../harness

@test "7888df83608e" {
  check 'z     :=y     ;y    :=     -1     * 4    ' '⇒ skip; y := (-1*4), {z → 0}
⇒ y := (-1*4), {z → 0}
⇒ skip, {y → -4, z → 0}'
}
