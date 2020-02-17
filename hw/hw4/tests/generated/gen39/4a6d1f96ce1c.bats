load ../../harness

@test "4a6d1f96ce1c" {
  check 'Rs    := -2     - x    ;x     :=   -1' '⇒ skip; x := -1, {Rs → -2}
⇒ x := -1, {Rs → -2}
⇒ skip, {Rs → -2, x → -1}'
}
