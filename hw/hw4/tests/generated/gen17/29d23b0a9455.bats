load ../../harness

@test "29d23b0a9455" {
  check 'if (¬(-3-2  <    4*     -4)) then y    :=  y     * LS  else skip ' '⇒ y := (y*LS), {}
⇒ skip, {y → 0}'
}
