load ../../harness

@test "e8ec32237be3" {
  check 'skip     ;x     :=   -3   *   -3' '⇒ x := (-3*-3), {}
⇒ skip, {x → 9}'
}
