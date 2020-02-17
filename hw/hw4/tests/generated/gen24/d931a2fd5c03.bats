load ../../harness

@test "d931a2fd5c03" {
  check 'x    :=   x     * -3    ;
skip     ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
