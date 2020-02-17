load ../../harness

@test "ffab6f47f421" {
  check 'skip     ;z :=  3     -     G   ' '⇒ z := (3-G), {}
⇒ skip, {z → 3}'
}
