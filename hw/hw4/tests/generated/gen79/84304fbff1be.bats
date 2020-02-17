load ../../harness

@test "84304fbff1be" {
  check 'skip     ;     z:= y   * y ' '⇒ z := (y*y), {}
⇒ skip, {z → 0}'
}
