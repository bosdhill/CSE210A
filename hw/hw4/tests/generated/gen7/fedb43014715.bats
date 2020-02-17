load ../../harness

@test "fedb43014715" {
  check 'h   :=     -1+   4     ;     lh  :=     -4+ 0   ' '⇒ skip; lh := (-4+0), {h → 3}
⇒ lh := (-4+0), {h → 3}
⇒ skip, {h → 3, lh → -4}'
}
