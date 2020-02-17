load ../../harness

@test "3cfc224db937" {
  check 'while x --1  <  xH     +    y    do skip' '⇒ skip, {}'
}
