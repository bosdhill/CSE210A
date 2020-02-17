load ../../harness

@test "01c43bd25f92" {
  check 'x   := 1+    y   ;
skip  ' '⇒ skip; skip, {x → 1}
⇒ skip, {x → 1}'
}
