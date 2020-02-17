load ../../harness

@test "f13e43a7b631" {
  check 'x     :=     IS     *0     ;y   := x  +     3' '⇒ skip; y := (x+3), {x → 0}
⇒ y := (x+3), {x → 0}
⇒ skip, {x → 0, y → 3}'
}
