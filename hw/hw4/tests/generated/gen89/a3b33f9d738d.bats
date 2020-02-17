load ../../harness

@test "a3b33f9d738d" {
  check 'skip  ;x  := -3     *  -2  ' '⇒ x := (-3*-2), {}
⇒ skip, {x → 6}'
}
