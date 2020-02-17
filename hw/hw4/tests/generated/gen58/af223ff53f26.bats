load ../../harness

@test "af223ff53f26" {
  check 'z     :=     4+ -3   ;
y   :=     z     -     3 ' '⇒ skip; y := (z-3), {z → 1}
⇒ y := (z-3), {z → 1}
⇒ skip, {y → -2, z → 1}'
}
