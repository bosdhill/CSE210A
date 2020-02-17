load ../../harness

@test "eded6e03f24d" {
  check 'y  := 4  -   x ;Q:=  L' '⇒ skip; Q := L, {y → 4}
⇒ Q := L, {y → 4}
⇒ skip, {Q → 0, y → 4}'
}
