load ../../harness

@test "6b7a97065d5e" {
  check 'skip   ;z     :=     y+ x  ' '⇒ z := (y+x), {}
⇒ skip, {z → 0}'
}
