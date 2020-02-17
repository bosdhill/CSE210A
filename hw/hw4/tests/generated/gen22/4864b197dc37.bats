load ../../harness

@test "4864b197dc37" {
  check 'y   := 3   *   x   ; D  :=    x  ' '⇒ skip; D := x, {y → 0}
⇒ D := x, {y → 0}
⇒ skip, {D → 0, y → 0}'
}
