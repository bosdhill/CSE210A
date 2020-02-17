load ../../harness

@test "91cc2914ab47" {
  check 'w7    :=     4;z :=G  +-3    ' '⇒ skip; z := (G+-3), {w7 → 4}
⇒ z := (G+-3), {w7 → 4}
⇒ skip, {w7 → 4, z → -3}'
}
