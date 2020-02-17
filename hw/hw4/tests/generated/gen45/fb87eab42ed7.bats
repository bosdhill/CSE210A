load ../../harness

@test "fb87eab42ed7" {
  check 'skip  ;  x   :=    -1   * x' '⇒ x := (-1*x), {}
⇒ skip, {x → 0}'
}
