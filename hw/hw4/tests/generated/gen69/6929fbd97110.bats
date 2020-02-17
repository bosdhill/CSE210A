load ../../harness

@test "6929fbd97110" {
  check 'x :=Z-  -3     ;y    :=   0     ' '⇒ skip; y := 0, {x → 3}
⇒ y := 0, {x → 3}
⇒ skip, {x → 3, y → 0}'
}
