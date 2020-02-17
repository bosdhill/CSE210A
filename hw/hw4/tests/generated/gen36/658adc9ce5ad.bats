load ../../harness

@test "658adc9ce5ad" {
  check 'x    :=     x     +y' '⇒ skip, {x → 0}'
}
