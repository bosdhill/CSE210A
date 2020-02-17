load ../../harness

@test "74218435fd62" {
  check 'z  :=2    -     x     ;skip ' '⇒ skip; skip, {z → 2}
⇒ skip, {z → 2}'
}
