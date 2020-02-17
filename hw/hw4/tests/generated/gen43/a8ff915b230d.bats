load ../../harness

@test "a8ff915b230d" {
  check 'skip     ; y   :=    y   *    x  ' '⇒ y := (y*x), {}
⇒ skip, {y → 0}'
}
