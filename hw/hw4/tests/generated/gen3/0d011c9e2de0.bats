load ../../harness

@test "0d011c9e2de0" {
  check 's   :=G  + -2    ;y :=  y   ' '⇒ skip; y := y, {s → -2}
⇒ y := y, {s → -2}
⇒ skip, {s → -2, y → 0}'
}
