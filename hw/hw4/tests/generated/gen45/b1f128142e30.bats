load ../../harness

@test "b1f128142e30" {
  check 'x     :=  g   + 3' '⇒ skip, {x → 3}'
}
