load ../../harness

@test "838867620d55" {
  check 'skip;y :=     -2    -   Fk  ' '⇒ y := (-2-Fk), {}
⇒ skip, {y → -2}'
}
