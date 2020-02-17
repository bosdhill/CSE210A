load ../../harness

@test "cc081db77340" {
  check 'skip    ;     YD    := z    *    z    ' '⇒ YD := (z*z), {}
⇒ skip, {YD → 0}'
}
