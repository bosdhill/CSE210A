load ../../harness

@test "a3408c57f80c" {
  check 'x    :=   z *  -4    ' '⇒ skip, {x → 0}'
}
