load ../../harness

@test "cc1773599f37" {
  check 'Z0   :=   -1    -   -2    ;x  :=  1   * -2     ' '⇒ skip; x := (1*-2), {Z0 → 1}
⇒ x := (1*-2), {Z0 → 1}
⇒ skip, {Z0 → 1, x → -2}'
}
