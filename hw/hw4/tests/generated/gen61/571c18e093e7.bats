load ../../harness

@test "571c18e093e7" {
  check 'y   := N    +    z     ' '⇒ skip, {y → 0}'
}
