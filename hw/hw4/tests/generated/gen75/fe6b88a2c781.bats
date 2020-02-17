load ../../harness

@test "fe6b88a2c781" {
  check 'G   :=   y    -     x     ;  y :=     y + -1     ' '⇒ skip; y := (y+-1), {G → 0}
⇒ y := (y+-1), {G → 0}
⇒ skip, {G → 0, y → -1}'
}
