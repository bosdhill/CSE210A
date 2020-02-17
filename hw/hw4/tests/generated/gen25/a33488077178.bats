load ../../harness

@test "a33488077178" {
  check 'y :=    y +    0    ' '⇒ skip, {y → 0}'
}
