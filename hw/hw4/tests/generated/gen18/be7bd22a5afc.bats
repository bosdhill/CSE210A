load ../../harness

@test "be7bd22a5afc" {
  check 'Q:=z +     -2;skip    ' '⇒ skip; skip, {Q → -2}
⇒ skip, {Q → -2}'
}
