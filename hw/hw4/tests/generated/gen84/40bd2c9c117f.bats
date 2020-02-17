load ../../harness

@test "40bd2c9c117f" {
  check 'if (true    ∧ -4  <4     +   2)      then L   :=   -2   else      B  := -1 -     z ' '⇒ L := -2, {}
⇒ skip, {L → -2}'
}
