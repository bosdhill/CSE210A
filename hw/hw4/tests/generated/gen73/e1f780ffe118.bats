load ../../harness

@test "e1f780ffe118" {
  check 'skip;y   :=    x ' '⇒ y := x, {}
⇒ skip, {y → 0}'
}
