load ../../harness

@test "a5b6287cf34b" {
  check 'skip    ;  y := t1     * 3 ' '⇒ y := (t1*3), {}
⇒ skip, {y → 0}'
}
