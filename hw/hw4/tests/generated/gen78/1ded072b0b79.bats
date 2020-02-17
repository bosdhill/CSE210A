load ../../harness

@test "1ded072b0b79" {
  check 'z    := -2   + bS  ;  z     :=j  -     x    ' '⇒ skip; z := (j-x), {z → -2}
⇒ z := (j-x), {z → -2}
⇒ skip, {z → 0}'
}
