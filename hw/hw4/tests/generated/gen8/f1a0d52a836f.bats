load ../../harness

@test "f1a0d52a836f" {
  check 'y :=     -1    +   4  ; y     :=  4    ' '⇒ skip; y := 4, {y → 3}
⇒ y := 4, {y → 3}
⇒ skip, {y → 4}'
}
