load ../../harness

@test "0e09e415c989" {
  check 'y :=  3   +z  ; y    :=    k  -   4    ' '⇒ skip; y := (k-4), {y → 3}
⇒ y := (k-4), {y → 3}
⇒ skip, {y → -4}'
}
