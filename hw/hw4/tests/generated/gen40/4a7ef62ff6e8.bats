load ../../harness

@test "4a7ef62ff6e8" {
  check 'x  :=1     -  -1;skip ' '⇒ skip; skip, {x → 2}
⇒ skip, {x → 2}'
}
