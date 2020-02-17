load ../../harness

@test "7e78a88c7a8d" {
  check 'x   :=1   +   x; A    :=    4+    y     ' '⇒ skip; A := (4+y), {x → 1}
⇒ A := (4+y), {x → 1}
⇒ skip, {A → 4, x → 1}'
}
