load ../../harness

@test "a6d8673fb4c2" {
  check 'm9:=   -1+  -4; y   := n   +     x     ' '⇒ skip; y := (n+x), {m9 → -5}
⇒ y := (n+x), {m9 → -5}
⇒ skip, {m9 → -5, y → 0}'
}
