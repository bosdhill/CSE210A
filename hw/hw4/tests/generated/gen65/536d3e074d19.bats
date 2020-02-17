load ../../harness

@test "536d3e074d19" {
  check 'x  := x ' '⇒ skip, {x → 0}'
}
