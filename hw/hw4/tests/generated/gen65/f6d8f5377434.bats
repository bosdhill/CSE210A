load ../../harness

@test "f6d8f5377434" {
  check 'YS :=   a    + z   ;y     :=     -4    +  z ' '⇒ skip; y := (-4+z), {YS → 0}
⇒ y := (-4+z), {YS → 0}
⇒ skip, {YS → 0, y → -4}'
}
