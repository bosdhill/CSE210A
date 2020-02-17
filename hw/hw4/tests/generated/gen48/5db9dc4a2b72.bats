load ../../harness

@test "5db9dc4a2b72" {
  check 'skip  ;y    :=  z     ' '⇒ y := z, {}
⇒ skip, {y → 0}'
}
