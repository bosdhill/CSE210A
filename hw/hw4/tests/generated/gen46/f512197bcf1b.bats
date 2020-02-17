load ../../harness

@test "f512197bcf1b" {
  check 'G    :=   -4     --2   ; y     :=  x ' '⇒ skip; y := x, {G → -2}
⇒ y := x, {G → -2}
⇒ skip, {G → -2, y → 0}'
}
