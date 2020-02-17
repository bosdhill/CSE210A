load ../../harness

@test "41edb1d2449d" {
  check 'x  :=x    *y     ;     x :=     -3+   x' '⇒ skip; x := (-3+x), {x → 0}
⇒ x := (-3+x), {x → 0}
⇒ skip, {x → -3}'
}
