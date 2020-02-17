load ../../harness

@test "ff3859e35954" {
  check 'x:=     3 -   ur ;y     :=  -2    ' '⇒ skip; y := -2, {x → 3}
⇒ y := -2, {x → 3}
⇒ skip, {x → 3, y → -2}'
}
