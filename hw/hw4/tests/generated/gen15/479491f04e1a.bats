load ../../harness

@test "479491f04e1a" {
  check 'H :=   2-  Y5 ;x     :=4   ' '⇒ skip; x := 4, {H → 2}
⇒ x := 4, {H → 2}
⇒ skip, {H → 2, x → 4}'
}
