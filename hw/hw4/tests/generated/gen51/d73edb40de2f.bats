load ../../harness

@test "d73edb40de2f" {
  check 'skip    ; y     :=    x- r' '⇒ y := (x-r), {}
⇒ skip, {y → 0}'
}
