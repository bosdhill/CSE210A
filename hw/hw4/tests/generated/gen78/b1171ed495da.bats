load ../../harness

@test "b1171ed495da" {
  check 'x  :=z    +     y;skip  ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
