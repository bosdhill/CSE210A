load ../../harness

@test "34952975269e" {
  check 'x   :=  zv  +1 ;y    :=1' '⇒ skip; y := 1, {x → 1}
⇒ y := 1, {x → 1}
⇒ skip, {x → 1, y → 1}'
}
