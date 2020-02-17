load ../../harness

@test "d76106475217" {
  check 'g   := x     -x;

y    :=    2     * x ' '⇒ skip; y := (2*x), {g → 0}
⇒ y := (2*x), {g → 0}
⇒ skip, {g → 0, y → 0}'
}
