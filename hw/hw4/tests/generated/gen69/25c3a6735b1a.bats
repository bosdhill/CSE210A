load ../../harness

@test "25c3a6735b1a" {
  check 'z   :=     z  *   z  ; y    :=    4    +-3 ' '⇒ skip; y := (4+-3), {z → 0}
⇒ y := (4+-3), {z → 0}
⇒ skip, {y → 1, z → 0}'
}
