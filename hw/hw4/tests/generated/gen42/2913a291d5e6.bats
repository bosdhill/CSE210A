load ../../harness

@test "2913a291d5e6" {
  check 'skip   ;x   :=2     -   w6' '⇒ x := (2-w6), {}
⇒ skip, {x → 2}'
}
