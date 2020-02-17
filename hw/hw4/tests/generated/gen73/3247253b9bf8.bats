load ../../harness

@test "3247253b9bf8" {
  check 'x     :=z*   x    ' '⇒ skip, {x → 0}'
}
