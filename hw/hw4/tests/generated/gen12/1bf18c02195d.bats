load ../../harness

@test "1bf18c02195d" {
  check 'skip     ;x   :=     y     *   x ' '⇒ x := (y*x), {}
⇒ skip, {x → 0}'
}
