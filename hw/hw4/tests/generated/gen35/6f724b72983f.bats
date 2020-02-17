load ../../harness

@test "6f724b72983f" {
  check 'y    :=     w   -  l;skip' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
