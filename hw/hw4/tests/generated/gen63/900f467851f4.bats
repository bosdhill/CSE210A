load ../../harness

@test "900f467851f4" {
  check 'x    :=  1     * 0  ' '⇒ skip, {x → 0}'
}
