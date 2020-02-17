load ../../harness

@test "6474a9ad8ca7" {
  check 'skip  ; x     :=     -4    *     -3    ' '⇒ x := (-4*-3), {}
⇒ skip, {x → 12}'
}
