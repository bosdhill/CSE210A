load ../../harness

@test "6d51ba3cc6b5" {
  check 'y :=z - z   ;y   :=     3 ' '⇒ skip; y := 3, {y → 0}
⇒ y := 3, {y → 0}
⇒ skip, {y → 3}'
}
