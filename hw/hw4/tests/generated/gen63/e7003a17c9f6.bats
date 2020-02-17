load ../../harness

@test "e7003a17c9f6" {
  check 'skip ;y   :=     af   + 2  ' '⇒ y := (af+2), {}
⇒ skip, {y → 2}'
}
