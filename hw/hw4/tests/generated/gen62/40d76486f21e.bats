load ../../harness

@test "40d76486f21e" {
  check 'z :=x    -    y     ' '⇒ skip, {z → 0}'
}
