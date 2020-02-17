load ../../harness

@test "d056741a72a9" {
  check 'V     :=     x;y   :=    y  ' '⇒ skip; y := y, {V → 0}
⇒ y := y, {V → 0}
⇒ skip, {V → 0, y → 0}'
}
