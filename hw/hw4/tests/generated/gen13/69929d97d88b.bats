load ../../harness

@test "69929d97d88b" {
  check 'skip  ; z     := -3 * 0  ' '⇒ z := (-3*0), {}
⇒ skip, {z → 0}'
}
