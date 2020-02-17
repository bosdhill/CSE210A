load ../../harness

@test "9043bbe97648" {
  check 'y     :=     0   *     x    ; y     :=     1   -     y' '⇒ skip; y := (1-y), {y → 0}
⇒ y := (1-y), {y → 0}
⇒ skip, {y → 1}'
}
