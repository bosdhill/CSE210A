load ../../harness

@test "cb41479da12c" {
  check 'skip; z    :=     2    + 4' '⇒ z := (2+4), {}
⇒ skip, {z → 6}'
}
