load ../../harness

@test "bb8ee6a2d88f" {
  check 'x   :=   x     +    x' '⇒ skip, {x → 0}'
}
