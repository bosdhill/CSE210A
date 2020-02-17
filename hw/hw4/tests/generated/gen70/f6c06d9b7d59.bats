load ../../harness

@test "f6c06d9b7d59" {
  check 'if (z     +     4   <   1  +     -4)     then     skip   else z     :=  0   + 1' '⇒ z := (0+1), {}
⇒ skip, {z → 1}'
}
