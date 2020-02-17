load ../../harness

@test "14b269625f32" {
  check 'skip;z     :=   y     +0   ' '⇒ z := (y+0), {}
⇒ skip, {z → 0}'
}
