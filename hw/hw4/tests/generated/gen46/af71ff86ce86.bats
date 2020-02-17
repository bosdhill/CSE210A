load ../../harness

@test "af71ff86ce86" {
  check 'if (¬false)   then  z  :=y     +   1      else skip     ' '⇒ z := (y+1), {}
⇒ skip, {z → 1}'
}
