load ../../harness

@test "2a5b8b84d28f" {
  check 'y     :=z   +    x   ' '⇒ skip, {y → 0}'
}
