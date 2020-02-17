load ../../harness

@test "86196bf4256c" {
  check 'z  :=   x;l  :=  -2  ' '⇒ skip; l := -2, {z → 0}
⇒ l := -2, {z → 0}
⇒ skip, {l → -2, z → 0}'
}
